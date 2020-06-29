// *** or ** means the length of these arrays MUST be set carefully
//     and depends on the specifics of the app


//                  (three.js - r.111  - 09 Dec 2019)

const URLS = [ "/nausten", "/nwhit" ];  // ***
var data = [];
var results = [];
var meta;
var num;

var container, renderer, scene;
var picData = [], picScene, picRT, mouse;
var cameraA;
var camerasB = [];
var controlsA, controlsB, stats, ptStats;
var pointsOnScreen = 0;

var geometries = [], picGeometries = [];
var myMaterial, myPicMaterial;
var particles = [], picParticles = [];
var cloud, picCloud, initCloudQuaternion;
const dROT = 0.0005;
var dRotCloud = dROT;
var rId = 0, rData, picPrint = "-";


// var colors = [ 0xff1111, 0x1111ff ];  // **
var timer = 0;
const duration = 360;
var currCoordSys = 0; // 0:Kartesian, 1:Cylindrical, 2:Spherical
var prevCoordSys = 0;
var coordSys = [ 1.0, 0.0, 0.0 ];
const SIZE = 2.0;

var nMaxs = [ 0, 0 ];  //**
var syncFrom = [ 0, 0 ];    //**
var syncTo = [ 100, 100 ];  //**
var asyncFrom = [ 0, 0 ];   //**
var asyncTo = [ 100, 100 ]; //**
var currFrom, currTo, start, count;

var flagPause = false;
var flagFullScreen = true;
var flagGrid = false;
var flagSync = true;
var flagAsync = false;
var flagPic = false;

const loadin = document.querySelector( '#loadin' );
const info = document.querySelector( '#info' );
const rangesArea = document.querySelector( '#ranges-area' );
const syncRangeArea = document.querySelector( '#sync-range' );
const asyncRangeArea = document.querySelector( '#async-range' ); //**
const screenMenu = document.querySelector( '#screen-menu' );

const buttonFS = document.querySelector( '#fullScreen' );
const buttonGS = document.querySelector( '#grid' );
const buttonKAR = document.querySelector( '#kartesian' );
const buttonCYL = document.querySelector( '#cylindrical' );
const buttonSPH = document.querySelector( '#spherical' );
const buttonSYNC = document.querySelector( '#syncronize' );
const buttonASYNC = document.querySelector( '#desyncronize' );
const buttonRESET = document.querySelector( '#reset-button' );
const buttonPAUSE = document.querySelector( '#pause-button' );

// // Listener
document.addEventListener( "DOMContentLoaded", function () {
    doAjaxThings(URLS);
} );

// // AJAX
async function doAjaxThings( urls ) {
    var l = urls.length;
    for ( let u = 0; u < l; u++ ) {
        var result = await makeRequest( "GET", urls[u] );
        results.push( result );
    }

    // // when data finishes loading
    var rl = results.length;
    for ( var r = 0; r < rl; r++ ) {
        var dat = JSON.parse( results[r] );
        data.push( dat );
    }

    loadin.style.display = 'none';
    info.style.display = 'block';
    rangesArea.style.display = 'block';
    screenMenu.style.display = 'block';

    doIt();
}

function makeRequest( method, url ) {
    return new Promise(( resolve, reject ) => {
        const xhr = new XMLHttpRequest();
        var byCache = ((/\?/).test(url) ? "&" : "?") + (new Date()).getTime();
        xhr.open( method, url + byCache, true );
        xhr.onload = () => resolve( xhr.response );
        xhr.onerror = () => reject( xhr.statusText );
        xhr.send();
    } );
}


function doIt() {
    init();
    play();
}

function init() {

    mouse = new THREE.Vector2();
    num = data.length;

    // // container
    container = document.querySelector( '#scene-container' );

    // // scene
    scene = new THREE.Scene();
    const fogHex = 0x000000;
    const fogDensity = 0.00007;
    scene.fog = new THREE.FogExp2( fogHex, fogDensity );

    picScene = new THREE.Scene();
    picScene.background = new THREE.Color( 0 ); // important!

    createCameras();
    createAndAddObjects();
    createRenderer();
    createRT();
    createControls();

    initStats();
    initAppendix();
    initPointStats();
    initJSR();
    addListeners();

    // // init all layers
    cameraA.layers.enableAll();
    camerasB[0].layers.enableAll();

    // dispose picking for start
    picScene.dispose();
    picRT.dispose();

}

function createCameras() {
    const fov = 35;
    const aspect = container.clientWidth / container.clientHeight;
    const near = 0.01;
    const far = 20;

    cameraA = new THREE.PerspectiveCamera( fov, aspect, near, far );
    cameraA.position.set( 0.5, 0.5, 5 );

    // // must num < 32 (three.js doc on layers) ***
    for ( var lay = 0; lay < num; lay++ ) {
        cameraA.layers.enable( lay );
    }

    for ( let c = 0; c < ( num + 1 ); c++ ) {
        var camera = new THREE.PerspectiveCamera( fov, aspect, near, far );
        camera.position.set( 0.5, 0.5, 5 );
        camerasB.push( camera );
    }

    for ( var lay = 0; lay < num; lay++ ) {
        camerasB[0].layers.enable( lay );
    }

}

function createControls() {
    controlsA = new THREE.TrackballControls( cameraA, container );
    controlsB = new THREE.TrackballControls( camerasB[0], container );
}

function createAndAddObjects() {

    // // the following matrix is the
    // //  (symmetric) 4D/float/customed-for-additive-blending
    // // ...version of
    // // [ 1, 0, 0,
    // //   0, 0, 1,
    // //   0, 1, 0 ], which is a permutation of the Identity.

    var myMatrix = new THREE.Matrix4();
    myMatrix.set( 1.0, 0.1, 0.1, 1.0,
                  0.1, 0.1, 1.0, 1.0,
                  0.1, 1.0, 0.1, 1.0,
                  1.0, 1.0, 1.0, 1.0 );

    cloud = new THREE.Group();
    picCloud = new THREE.Group();
    var ip = 0;
    picData[0] = {
        belongs: -1,
        localIndex: -1,
    };

    // // geometries ...vertices in the range [0,1]
    for (let g = 0; g < num; g++ ) {
        var cloudSize = data[g].length;

        var positions = [];
        var sizes = [];
        var defColors = [];
        var picColors = [];
        var colorP = new THREE.Color();

        var vertex = new THREE.Vector3();
        var colorV = new THREE.Vector3();

        for ( let i = 0; i < cloudSize; i++ ) {
            vertex.x = data[g][i][0];
            vertex.y = data[g][i][1];
            vertex.z = data[g][i][2];

            positions.push( vertex.x, vertex.y, vertex.z );
            sizes.push( 1.0 );

            colorV.setFromMatrixColumn( myMatrix, g );
            defColors.push( colorV.x, colorV.y, colorV.z );

            colorP.setHex( ip+1 );
            picColors.push( colorP.r, colorP.g, colorP.b );
            picData[ ip+1 ] = {
                belongs: g,
                localIndex: i,
            };
            ip++;
        }
        var geometry = new THREE.BufferGeometry();

        // // the typed BufferAttributes like Float32BufferAttribute are
        // // just a wrapper around BufferAttributes that also do
        // // normalization on the arrays, i.e. BEWARE on what you pass to them!

        var positionAttr = new THREE.Float32BufferAttribute( positions, 3 );
        geometry.setAttribute( 'position', positionAttr );

        var colorParamAttr = new THREE.Float32BufferAttribute( defColors, 3 );
        geometry.setAttribute( 'color', colorParamAttr );

        var sizeAttr = new THREE.Float32BufferAttribute( sizes, 1 );
        geometry.setAttribute( 'size', sizeAttr );

        var pColor = new THREE.Float32BufferAttribute( picColors, 3 );
        geometry.setAttribute( 'pic', pColor );

        geometries.push( geometry );

        var pGeometry = geometry.clone();
        picGeometries.push( pGeometry );
    }

    // // material
    var vert = document.getElementById( 'vShader' ).textContent;
    var frag = document.getElementById( 'fShader' ).textContent;

    myMaterial = new THREE.RawShaderMaterial( {
        uniforms: {
            uK: { value: coordSys[0] },
            uC: { value: coordSys[1] },
            uS: { value: coordSys[2] },
        },
        vertexShader: vert,
        fragmentShader: frag,
        blending: THREE.AdditiveBlending,
        depthWrite: false,
        depthTest: false,
        // alphaTest: 0.5,
    } );

    myPicMaterial = new THREE.RawShaderMaterial( {
        uniforms: {
            uK: { value: coordSys[0] },
            uC: { value: coordSys[1] },
            uS: { value: coordSys[2] },
        },
        vertexShader: "#define PICKING\n" + vert,
        fragmentShader: "#define PICKING\n" + frag,
        blending: THREE.NoBlending,
        depthWrite: false,
        depthTest: false,
        // alphaTest: 0.5,
    } );

    // // particles
    for ( let p = 0; p < num; p++ ) {
        particles.push( new THREE.Points( geometries[p], myMaterial ) );
        particles[p].layers.set( p );
        particles[p].frustumCulled = false;
        cloud.add( particles[p] );

        picParticles.push( new THREE.Points( geometries[p], myPicMaterial ) );
        picParticles[p].layers.set( p );
        picParticles[p].frustumCulled = false;
        picCloud.add( picParticles[p] );

        nMaxs[p] = particles[p].geometry.attributes.position.count;
    }

    // // Box helper
    var boxG = new THREE.BoxBufferGeometry( 2, 2, 2 );
    var edges = new THREE.EdgesGeometry( boxG );
    var line = new THREE.LineSegments( edges );
    line.material.depthTest = false;
    line.material.opacity = 0.25;
    line.material.transparent = true;
    line.material.color = new THREE.Color( 'grey' );
    line.layers.enableAll();
    // cloud.add( line );

    // // axes helper
    var axesHelperA = new THREE.AxesHelper( 100 );
    axesHelperA.layers.enableAll();
    // cloud.add( axesHelperA );

    initCloudQuaternion = new THREE.Quaternion();
    initCloudQuaternion.copy( cloud.quaternion );

    scene.add( cloud );
    picScene.add( picCloud );

}

function createRenderer() {

    renderer = new THREE.WebGLRenderer( {
        antialias: false,
        // premultipliedAlpha: false,  // false is the default already...
        stencil: false,
    } );

    renderer.setSize( container.clientWidth, container.clientHeight );
    renderer.setPixelRatio( window.devicePixelRatio );
    renderer.setClearColor( 0 );
    renderer.autoClear = false;

    renderer.gammaFactor = 2.2;
    renderer.gammaOutput = true;

    container.appendChild ( renderer.domElement );

    renderer.domElement.addEventListener( 'mousemove', onMouseMove );
}

function createRT() {
    var options = {
        format: THREE.RGBAFormat,
        type: THREE.UnsignedByteType,
        anisotropy: 1,
        magFilter: THREE.LinearFilter,
        minFilter: THREE.NearestFilter,
        depthBuffer: false,
        stencilBuffer: false
    };

    picRT = new THREE.WebGLRenderTarget( 1, 1, options );
    picRT.texture.generateMipmaps = false;
}

function onMouseMove( e ) {
    mouse.x = e.clientX;
    mouse.y = e.clientY;
}

function play() {
    flagPause = true;
    renderer.setAnimationLoop( () => {

        update();
        render();
        stats.update();
        ptStats.update();
    } );

}

function update() {
    // perform updates to the scene, called once per frame (AVOID)

    if ( timer <= 1 ) {
        coordSys = [ 0.0, 0.0, 0.0 ];
        coordSys[prevCoordSys] = 0.0;
        coordSys[currCoordSys] = 1.0;
        timer = 0;
    } else {
        coordSys = [ 0.0, 0.0, 0.0 ];
        coordSys[prevCoordSys] = timer / duration;
        coordSys[currCoordSys] = ( duration - timer ) / duration;
        timer -= 1;
    }
    myMaterial.uniforms.uK.value = coordSys[0];
    myMaterial.uniforms.uC.value = coordSys[1];
    myMaterial.uniforms.uS.value = coordSys[2];
    myPicMaterial.uniforms.uK.value = coordSys[0];
    myPicMaterial.uniforms.uC.value = coordSys[1];
    myPicMaterial.uniforms.uS.value = coordSys[2];

    if ( flagSync ) {
        currFrom = syncFrom;
        currTo = syncTo;
    }
    if ( flagAsync ) {
        currFrom = asyncFrom;
        currTo = asyncTo;
    }
    for ( let i = 0; i < particles.length; i++ ) {
        start = Math.round(nMaxs[i] * currFrom[i] / 100);
        count = Math.round(nMaxs[i] * (currTo[i] - currFrom[i]) / 100);
        particles[i].geometry.setDrawRange( start, count );
        picParticles[i].geometry.setDrawRange( start, count );
    }

    cloud.rotation.y -= dRotCloud;
    picCloud.quaternion.copy( cloud.quaternion );

    if ( controlsA.enabled ) {
        controlsA.update();
    }
    if ( controlsB.enabled ) {
        controlsB.update();
    }
}

function render() {

    renderer.clear();
    pointsOnScreen = 0;

    if ( flagFullScreen ) {
        if ( flagPic ) {
            pick();
        }
        renderer.setViewport( 0,0,container.clientWidth,container.clientHeight);
        renderer.render( scene, cameraA );
        pointsOnScreen += renderer.info.render.points;
    }

    if ( flagGrid ) {

        // if ( num === 1 ) {
        //     sliX = container.clientWidth;
        //     sliY = container.clientHeight;

        //     renderer.setViewport( 0, 0, sliX, sliY );
        //     renderer.render( scene, camerasB[0] );
        // }

        // if ( num === 2 ) {
        var sliX = container.clientWidth / 2;
        var sliY = container.clientHeight / 2;

        renderer.setViewport( sliX / 2, 0, sliX, sliY );
        renderer.render( scene, camerasB[0] );
        pointsOnScreen += renderer.info.render.points;

        renderer.setViewport( 0, sliY, sliX, sliY );
        camerasB[1].position.copy( camerasB[0].position );
        camerasB[1].quaternion.copy( camerasB[0].quaternion );
        camerasB[1].layers.enable( 0 );
        camerasB[1].layers.disable( 1 );
        renderer.render( scene, camerasB[1] );
        pointsOnScreen += renderer.info.render.points;

        renderer.setViewport( sliX, sliY, sliX, sliY );
        camerasB[2].position.copy( camerasB[0].position );
        camerasB[2].quaternion.copy( camerasB[0].quaternion );
        camerasB[2].layers.enable( 1 );
        camerasB[2].layers.disable( 0 );
        renderer.render( scene, camerasB[2] );
        pointsOnScreen += renderer.info.render.points;
        // }

        // if ( num == 3 ) {}
        // if ( num == 4 ) {}
        // if ( num == 5 ) {}
    }

}

function pick() {

    // // render the picScene off-screen
    cameraA.setViewOffset(
        renderer.domElement.width,
        renderer.domElement.height,
        mouse.x * window.devicePixelRatio | 0,
        mouse.y * window.devicePixelRatio | 0,
        1,
        1
    );

    // // render the picScene
    renderer.setRenderTarget( picRT );
    renderer.render( picScene, cameraA );

    // // clear the view offset
    cameraA.clearViewOffset();
    cameraA.updateProjectionMatrix();

    // // create buffer and read the pixel
    var pixelBuffer = new Uint8Array( 4 );
    renderer.readRenderTargetPixels(
        picRT,
        0,
        0,
        1,
        1,
        pixelBuffer
    );

    // // interpret pixel as read-id
    console.log( "bef", rId );
    rId =
        ( pixelBuffer[ 0 ] << 16 ) |
        ( pixelBuffer[ 1 ] << 8 ) |
        ( pixelBuffer[ 2 ] );
    console.log( "after", rId );

    if ( rId == 0 ) {              // (rId == 0)
        picPrint = "-";
    } else {                       // (rId > 0)
        rData = picData[ rId ];
        if ( rData ) {
            picPrint = rData.belongs + " " + rData.localIndex;
        }
    }

    renderer.setRenderTarget( null );
}

function initStats() {
    stats = new Stats();
    stats.showPanel( 0 );
    stats.dom.style.position = 'absolute';
    stats.dom.style.top = '0px';
    stats.dom.style.left = '0px';
    container.appendChild( stats.dom );
}

function initAppendix() {
    appendx = new ADAx.Appendix();
    appendx.domElement.style.position = 'absolute';
    appendx.domElement.style.bottom = '1px';
    appendx.domElement.style.left = '1px';
    info.appendChild( appendx.domElement );
}

function initPointStats() {
    ptStats = new ADAx.PointStats();
    ptStats.domElement.style.position = 'absolute';
    ptStats.domElement.style.top = '1px';
    container.appendChild( ptStats.domElement );
}

function addListeners() {

    window.addEventListener( 'resize', onWindowResize, false );

    document.addEventListener( 'keyup', onKeyUp, false);
    document.addEventListener( 'keydown', onKeyDown, false );

    buttonFS.addEventListener( 'click', function () {
        flagFullScreen = true;
        flagGrid = false;
        controlsA.enabled = true;
        controlsB.enabled = false;
    }, false );
    buttonGS.addEventListener( 'click', function () {
        flagFullScreen = false;
        flagGrid = true;
        controlsA.enabled = false;
        controlsB.enabled = true;
    }, false );

    buttonKAR.addEventListener( 'click', toKartesian, false );
    buttonCYL.addEventListener( 'click', toCylindrical, false );
    buttonSPH.addEventListener( 'click', toSpherical, false );

    buttonSYNC.addEventListener( 'click', function () {
        flagSync = true;
        flagAsync = false;
        syncRangeArea.style.display = 'block';
        asyncRangeArea.style.display = 'none';
    }, false );
    buttonASYNC.addEventListener( 'click', function () {
        flagSync = false;
        flagAsync = true;
        syncRangeArea.style.display = 'none';
        asyncRangeArea.style.display = 'block';
    }, false );

    buttonRESET.addEventListener( 'click', reset, false );
    buttonPAUSE.addEventListener( 'click', pause, false );
}

function toKartesian() {
    if ( ( currCoordSys !== 0 ) && ( timer === 0 ) ) {
        prevCoordSys = currCoordSys;
        timer = duration;
        currCoordSys = 0;
    }
}
function toCylindrical() {
    if ( ( currCoordSys !== 1 ) && ( timer === 0 ) ) {
        prevCoordSys = currCoordSys;
        timer = duration;
        currCoordSys = 1;
    }
}
function toSpherical() {
    if ( ( currCoordSys !==2 ) && ( timer === 0 ) ) {
        prevCoordSys = currCoordSys;
        timer = duration;
        currCoordSys = 2;
    }
}

function onWindowResize() {
    // // called upon every window resize (AVOID)

    // // set cameraA's aspect ratio to match that of the new window
    cameraA.aspect = container.clientWidth / container.clientHeight;
    cameraA.updateProjectionMatrix();

    // update other cameras ..keeping same aspect with cameraA
    for ( let c = 0; c < camerasB.length; c++ ) {
        camerasB[c].aspect = container.clientWidth / container.clientHeight;
        camerasB[c].updateProjectionMatrix();
    }

    // update the size of the renderer AND of the canvas
    renderer.setSize( container.clientWidth, container.clientHeight );
    renderer.setPixelRatio( window.devicePixelRatio );
}

function onKeyUp(e) {
    var ec = e.code;

    if ( ec === 'Space' ) {
        pause();
    }
    if ( ec === 'KeyR' ) {
        reset();
    }
    if ( ec === 'KeyP' ) {
        picIt();
    }
}

function onKeyDown(e) {
    var ec = e.code;

    if ( flagFullScreen ) {

        if ( ec === 'Digit1' ) {
            if ( e.shiftKey ) {
                cameraA.layers.disable( 0 );
            } else {
                cameraA.layers.enable( 0 );
            }
        }
        if ( ec === 'Digit2' ) {
            if ( e.shiftKey ) {
                cameraA.layers.disable( 1 );
            } else {
                cameraA.layers.enable( 1 );
            }
        }
        if ( ec === 'Digit9' ) {
            cameraA.layers.enableAll();
        }

    }
}

function reset() {
    cloud.quaternion.copy( initCloudQuaternion );
    controlsA.reset();
    controlsB.reset();
}

function pause() {
    if ( flagPause ) {
        flagPause = false;
        dRotCloud = 0;
    } else {
        flagPause = true;
        dRotCloud = dROT;
    }
}

function picIt() {
    if ( flagPic ) {
        flagPic = false;
        picScene.dispose();
        picRT.dispose();
        picPrint = "-";
    } else {
        flagPic = true;
    }
}

function stop() {
    if ( flagPause === true ) {
        flagPause = false;
        renderer.setAnimationLoop( null );
    } else {
        play();
    }
}

function initJSR() {

    if ( flagSync ) {
        syncRangeArea.style.display = 'block';
        asyncRangeArea.style.display = 'none';
    }
    if ( flagAsync ) {
        syncRangeArea.style.display = 'none';
        asyncRangeArea.style.display = 'block';
    }

    const syncRange = new JSR( ['#sync-from', '#sync-to'], {
        sliders: 2,
        values: [ syncFrom[0], syncTo[0] ],
        labels: {
            minMax: false,
            formatter: (value) => {
                return value.toString() + '%';
            },
        },
    } );
    syncRange.addEventListener( 'update', (input, value) => {
        if ( input.id == "sync-from" ) {
            syncFrom[0] = value;
            syncFrom[1] = value;
        }
        if ( input.id == "sync-to" ) {
            syncTo[0] = value;
            syncTo[1] = value;
        }
    } );

    const redRange = new JSR( ['#red-from', '#red-to'], {
        sliders: 2,
        values: [ asyncFrom[0], asyncTo[0] ],
        labels: {
            minMax: false,
            formatter: (value) => {
                return value.toString() + '%';
            },
        },
    } );
    redRange.addEventListener( 'update', (input, value) => {
        if ( input.id == "red-from" ) {
            asyncFrom[0] = value;
        }
        if ( input.id == "red-to" ) {
            asyncTo[0] = value;
        }
    } );

    const blueRange = new JSR( ['#blue-from', '#blue-to'], {
        sliders: 2,
        values: [ asyncFrom[1], asyncTo[1] ],
        labels: {
            minMax: false,
            formatter: (value) => {
                return value.toString() + '%';
            },
        },
    } );
    blueRange.addEventListener( 'update', (input, value) => {
        if ( input.id == "blue-from" ) {
            asyncFrom[1] = value;
        }
        if ( input.id == "blue-to" ) {
            asyncTo[1] = value;
        }
    } );
}

var ADAx = ADAx || {}

ADAx.Appendix = function() {

    var appendixContainer = document.createElement( 'div' );
    appendixContainer.style.cssText = 'width:auto;opacity:0.7;cursor:default';

    var apxDiv = document.createElement( 'div' );
    apxDiv.style.cssText = 'padding:0 0 0 0;text-align:left;background:transparent;';
    appendixContainer.appendChild( apxDiv );

    var apxTextA = document.createElement( 'div' );
    apxTextA.style.cssText = 'color:#f00;font-family:monospace,sans-serif;font-size:1vmin;font-weight:bold;line-height:1.1vmin';
    apxTextA.innerHTML = '';
    apxDiv.appendChild( apxTextA );

    var apxTextsA = [];
    var nLines = 11;  //**
    for( var i = 0; i < nLines; i++ ){
	apxTextsA[i] = document.createElement( 'div' );
	apxTextsA[i].style.cssText = 'color:#006699;background:transparent;font-family:sans-serif;font-size:1.2vmin;font-weight:normal;line-height:1.3vmin;white-space:nowrap;';
	apxDiv.appendChild( apxTextsA[i] );
	apxTextsA[i].innerHTML = '-';
    }
    var i = 0;
    apxTextsA[i++].textContent = "====== Controls ======";
    apxTextsA[i++].textContent = "A + LeftMouse: Orbiting";
    apxTextsA[i++].textContent = "S + LeftMouse: Zooming";
    apxTextsA[i++].textContent = "D + LeftMouse: Panning";
    apxTextsA[i++].textContent = "=== FullScreen ONLY ======";
    apxTextsA[i++].textContent = "Shift + 1 / 1: Hide/Show Red";
    apxTextsA[i++].textContent = "Shift + 2 / 2: Hide/Show Blue";
    apxTextsA[i++].textContent = "============ INFO ==============";
    apxTextsA[i++].textContent = "Word 3-Grams - little / log"; //***
    apxTextsA[i++].textContent = "Red: Austen - Sense..";           //***
    apxTextsA[i++].textContent = "Blue: Whitman - Leaves..";       //***
    // apxTextsA[i++].textContent = "\xa0\xa0\xa0\xa0\xa0\xa0Hide/Show Red";

    // apxTextsA[i++].textContent = "\xa0\xa0\xa0\xa0\xa0\xa0Hide/Show Blue";

    return {
	domElement: appendixContainer,
	}
};

ADAx.PointStats = function() {

    var pointStatsContainer = document.createElement( 'div' );
    pointStatsContainer.style.cssText = 'width:100%;opacity:0.9;cursor:default';

    var psDiv = document.createElement( 'div' );
    psDiv.style.cssText = 'text-align:center;';
    pointStatsContainer.appendChild( psDiv );

    var psTexts	= [];
    var nLines = 2;
    for ( var i = 0; i < nLines; i++ ) {
	psTexts[i] = document.createElement( 'div' );
	psTexts[i].style.cssText = 'color:#0099ff;background:transparent;font-family:monospace;font-size:9px;font-weight:bold;line-height:15px';
	psDiv.appendChild( psTexts[i] );
	psTexts[i].innerHTML= '-';
    }


    var lastTime = Date.now();
    return {
	domElement: pointStatsContainer,

	update: function() {

	    // refresh only 30time per second
	    if ( Date.now() - lastTime < 1000/30 ) return;
	    lastTime = Date.now()

	    var i = 0;
	    psTexts[i++].textContent = "Points: " + pointsOnScreen;
            psTexts[i++].textContent = picPrint;
	}
    }
};
