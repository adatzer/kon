// *** or ** means the length of these arrays MUST be set carefully

//                  (three.js - r.118)

// imports
import * as THREE from "https://cdn.jsdelivr.net/npm/three@0.118.3/build/three.module.js";
import { TrackballControls } from "https://cdn.jsdelivr.net/npm/three@0.118.3/examples/jsm/controls/TrackballControls.js";
import Stats from "https://cdn.jsdelivr.net/npm/three@0.118.3/examples/jsm/libs/stats.module.js";

// fun
const myfun = (urlarr, siz, ad) => {

    let data = [], results = [];
    let meta, num, recs = [];
    const numRECS = 3;
    const invokeURL = '{API}';

    let container, renderer, scene;
    let picData = [], picScene, picRT, mouse;
    let cameraA, camerasB = [];
    let controlsA, controlsB, stats, ptStats, appendx;
    let pointsOnScreen = 0;

    let geometries = [], picGeometries = [];
    let myMaterial, myPicMaterial;
    let particles = [], picParticles = [];
    let cloud, picCloud, initCloudQuaternion;
    const dROT = 0.0005;
    let dRotCloud = dROT;
    let rId = 0, rData, picPrint = "-";

    let timer = 0;
    const duration = 36;
    const coordinateSystems = [ "Cartesian", "Cylindrical", "Spherical" ];
    let currCoordSys = 0; // 0:Kartesian, 1:Cylindrical, 2:Spherical
    let prevCoordSys = 0;
    let coordSys = [ 1.0, 0.0, 0.0 ];
    const SIZE = siz;

    let nMaxs = [ 0, 0 ];       //**
    let syncFrom = [ 0, 0 ];    //**
    let syncTo = [ 100, 100 ];  //**
    let asyncFrom = [ 0, 0 ];   //**
    let asyncTo = [ 100, 100 ]; //**
    let currFrom, currTo, start, count;

    let flagPause = false;
    let flagFullScreen = true;
    let flagGrid = false;
    let flagSync = true;
    let flagAsync = false;
    let flagPic = false;

    let beginTime = Date.now(), prevTime = beginTime, frames = 0, fps = 0; // for FPS calc

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

    // // TRACKING init start
    var trackPauseWay = '';
    var trackResetWay = '';

    // Structured Events - se_category: Interaction
    //  1. Coordinate System (kartesian, cylindrical, spherical)
    //  2. Draw Range Style (sync, async)
    //  3. Screen Layout (full, grid)

    function trackCoordSys ( label, property, value ) {

        window.snowp_in_fun( 'trackStructEvent',
                             'Interaction',
                             'Coordinate System',
                             label,
                             property,
                             value );

    }

    function trackDrawRangeStyle ( label, property, value ) {

        window.snowp_in_fun( 'trackStructEvent',
                             'Interaction',
                             'Draw Range Style',
                             label,
                             property,
                             value );

    }

    function trackScreenLay ( label, property, value ) {

        window.snowp_in_fun( 'trackStructEvent',
                             'Interaction',
                             'Screen Layout',
                             label,
                             property,
                             value );

    }

    function argsContextGenerator ( args ) {

        var argsContext = {
            schema: 'iglu:test.adatzer.iglu/main_args/jsonschema/1-0-0',
            data: {
                point_size: siz,
                additive_blending: ad,
                num_clouds: urlarr.length
            }
        };

        return argsContext;

    }

    function stateContextGenerator ( args ) {

        var stateContext = {
            schema: 'iglu:test.adatzer.iglu/kon_state/jsonschema/1-0-0',
            data: {
                FPS: parseInt( fps ),
                points_on_screen: pointsOnScreen,
                coord_system: coordinateSystems[ currCoordSys ],
                screen_layout: flagFullScreen ? 'Full' : 'Grid',
                draw_range_style: flagSync ? 'Sync' : 'Async',
                sync_draw_range: {
                    from: syncFrom,
                    to: syncTo
                },
                async_draw_range: {
                    from: asyncFrom,
                    to: asyncTo
                }
            }
        };

        return stateContext;

    }

    function notPageViewEventFilter ( args ) {

        return args[ 'eventType' ] !== 'pv';

    }

    function onlyPageViewEventFilter ( args ) {

        return args[ 'eventType' ] === 'pv';

    }

    window.snowp_in_fun( 'addGlobalContexts',
                         [
                             [ notPageViewEventFilter, [ stateContextGenerator ] ],
                             [ onlyPageViewEventFilter, [ argsContextGenerator ] ]
                         ] );

    window.snowp_in_fun( 'enableActivityTracking', 10, 10 );

    window.snowp_in_fun('trackPageView',
                        null,
                        [{
                            schema: 'iglu:test.adatzer.iglu/pv_webgl/jsonschema/1-0-0',
                            data: {
                                needsWebGL: true
                            }
                        },
                         {
                             schema: 'iglu:test.adatzer.iglu/br_webgl/jsonschema/1-0-0',
                             data: {
                                 isWebGLAvailable: isWebGLAvail(),
                                 isWebGL2Available: isWebGL2Avail()
                             }
                         }]
                       );

    // helpers for detecting webgl support
    function isWebGLAvail () {

        try {

            var canv = document.createElement( 'canvas' );

            return !! ( window.WebGLRenderingContext && ( canv.getContext( 'webgl' ) || canv.getContext( 'experimental-webgl' ) ) );

        } catch ( e ) {

            return false;

        }

    }

    function isWebGL2Avail () {

        try {

            var canv = document.createElement( 'canvas' );

            return !! ( window.WebGL2RenderingContext && canv.getContext( 'webgl2' ) );

        } catch ( e ) {

            return false;

        }

    }

    // // TRACKING init end

    // // Listener
    document.addEventListener( "DOMContentLoaded", function () {
        doAjaxThings(urlarr, invokeURL);
    } );

    // // AJAX
    async function doAjaxThings( urls, apiGate ) {

        // get the coordinates
        let l = urls.length;
        for ( let u = 0; u < l; u++ ) {

            let result = await makeRequest( "GET", urls[u] );
            results.push( result );

        }

        let rl = results.length;
        for ( let r = 0; r < rl; r++ ) {

            let dat = JSON.parse( results[r] );
            data.push( dat );

        }

        // get the recommendations
        let requRecs = await makeRequest( "GET", apiGate );
        recs = JSON.parse( requRecs );
        recs = recs.slice( 0, numRECS);

        // displays
        loadin.style.display = 'none';
        info.style.display = 'block';
        rangesArea.style.display = 'block';
        screenMenu.style.display = 'block';

        // finally
        doIt();

    }


    function makeRequest( method, url ) {

        return new Promise(( resolve, reject ) => {

            const xhr = new XMLHttpRequest();
            // ...
            //let byCache = ((/\?/).test(url) ? "&" : "?") + (new Date()).getTime();
            let byCache = "";
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
        for ( let lay = 0; lay < num; lay++ ) {

            cameraA.layers.enable( lay );

        }

        for ( let c = 0; c < ( num + 1 ); c++ ) {

            let camera = new THREE.PerspectiveCamera( fov, aspect, near, far );
            camera.position.set( 0.5, 0.5, 5 );
            camerasB.push( camera );

        }

        for ( let lay = 0; lay < num; lay++ ) {

            camerasB[0].layers.enable( lay );

        }

    }


    function createControls() {

        controlsA = new TrackballControls( cameraA, container );
        controlsB = new TrackballControls( camerasB[0], container );

    }


    function createAndAddObjects() {

        // // the following matrix is the
        // //  (symmetric) 4D/float/customed-for-additive-blending
        // // ...version of
        // // [ 1, 0, 0,
        // //   0, 0, 1,
        // //   0, 1, 0 ], which is a permutation of the Identity.
        // // Its rows and columns match the RGB values of Red, Blue, Green

        let myMatrix = new THREE.Matrix4();
        myMatrix.set( 1.0,  ad,  ad, 1.0,
                       ad,  ad, 1.0, 1.0,
                       ad, 1.0,  ad, 1.0,
                      1.0, 1.0, 1.0, 1.0 );

        cloud = new THREE.Group();
        picCloud = new THREE.Group();
        let ip = 0;
        picData[0] = {
            belongs: -1,
            localIndex: -1,
        };

        // // geometries ...vertices in the range [0,1]
        for (let g = 0; g < num; g++ ) {

            let cloudSize = data[g].length;

            let positions = [];
            let defColors = [];
            let picColors = [];
            let colorP = new THREE.Color();

            let vertex = new THREE.Vector3();
            let colorV = new THREE.Vector3();

            for ( let i = 0; i < cloudSize; i++ ) {

                vertex.x = data[ g ][ i ][ 0 ];
                vertex.y = data[ g ][ i ][ 1 ];
                vertex.z = data[ g ][ i ][ 2 ];

                positions.push( vertex.x, vertex.y, vertex.z );

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

            let geometry = new THREE.BufferGeometry();

            // // the typed BufferAttributes like Float32BufferAttribute are
            // // just a wrapper around BufferAttributes that also do
            // // normalization on the arrays, i.e. BEWARE on what you pass to them!

            let positionAttr = new THREE.Float32BufferAttribute( positions, 3 );
            geometry.setAttribute( 'position', positionAttr );

            let colorParamAttr = new THREE.Float32BufferAttribute( defColors, 3 );
            geometry.setAttribute( 'color', colorParamAttr );

            let pColor = new THREE.Float32BufferAttribute( picColors, 3 );
            geometry.setAttribute( 'pic', pColor );

            geometries.push( geometry );

            let pGeometry = geometry.clone();
            picGeometries.push( pGeometry );

        }

        // // material
        let vert = document.getElementById( 'vShader' ).textContent;
        let frag = document.getElementById( 'fShader' ).textContent;

        myMaterial = new THREE.RawShaderMaterial( {
            uniforms: {
                uK: { value: coordSys[ 0 ] },
                uC: { value: coordSys[ 1 ] },
                uS: { value: coordSys[ 2 ] },
                uSize: { value: siz },
            },
            vertexShader: vert,
            fragmentShader: frag,
            blending: THREE.AdditiveBlending,
            depthWrite: false,
            depthTest: false,
        } );

        myPicMaterial = new THREE.RawShaderMaterial( {
            uniforms: {
                uK: { value: coordSys[ 0 ] },
                uC: { value: coordSys[ 1 ] },
                uS: { value: coordSys[ 2 ] },
                uSize: { value: siz },
            },
            vertexShader: "#define PICKING\n" + vert,
            fragmentShader: "#define PICKING\n" + frag,
            blending: THREE.NoBlending,
            depthWrite: false,
            depthTest: false,
        } );

        // // particles
        for ( let p = 0; p < num; p++ ) {

            particles.push( new THREE.Points( geometries[ p ], myMaterial ) );
            particles[p].layers.set( p );
            particles[p].frustumCulled = false;
            cloud.add( particles[ p ] );

            picParticles.push( new THREE.Points( geometries[ p ], myPicMaterial ) );
            picParticles[p].layers.set( p );
            picParticles[p].frustumCulled = false;
            picCloud.add( picParticles[ p ] );

            nMaxs[ p ] = particles[ p ].geometry.attributes.position.count;

        }

        // // Box helper
        // let boxG = new THREE.BoxBufferGeometry( 2, 2, 2 );
        // let edges = new THREE.EdgesGeometry( boxG );
        // let line = new THREE.LineSegments( edges );
        // line.material.depthTest = false;
        // line.material.opacity = 0.25;
        // line.material.transparent = true;
        // line.material.color = new THREE.Color( 'grey' );
        // line.layers.enableAll();
        // cloud.add( line );

        // // axes helper
        // let axesHelperA = new THREE.AxesHelper( 100 );
        // axesHelperA.layers.enableAll();
        // cloud.add( axesHelperA );

        initCloudQuaternion = new THREE.Quaternion();
        initCloudQuaternion.copy( cloud.quaternion );

        scene.add( cloud );
        picScene.add( picCloud );

    }


    function createRenderer() {

        // // in r118, WebGlRenderer creates WebGL2 (falls back to 1, if necessary)
        renderer = new THREE.WebGLRenderer( {
            antialias: false,
            // premultipliedAlpha: false,  // false is the default already...
            stencil: false,
        } );

        renderer.setSize( container.clientWidth, container.clientHeight );
        renderer.setPixelRatio( window.devicePixelRatio );
        renderer.setClearColor( 0 );
        renderer.autoClear = false;

        // renderer.gammaFactor = 2.2;
        // renderer.gammaOutput = true; // deprecated

        container.appendChild ( renderer.domElement );

        renderer.domElement.addEventListener( 'mousemove', onMouseMove );

    }


    // RenderTarget
    function createRT() {

        let options = {
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

            var now = Date.now();
            if ( now >= prevTime + 1000 ) {

                fps = ( frames * 1000 ) / ( now - prevTime );
                prevTime = now;
                frames = 0;

            }

        } );

    }


    function update() {
        // performs updates to the scene, called once per frame (AVOID)

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

        myMaterial.uniforms.uK.value = coordSys[ 0 ];
        myMaterial.uniforms.uC.value = coordSys[ 1 ];
        myMaterial.uniforms.uS.value = coordSys[ 2 ];
        myPicMaterial.uniforms.uK.value = coordSys[ 0 ];
        myPicMaterial.uniforms.uC.value = coordSys[ 1 ];
        myPicMaterial.uniforms.uS.value = coordSys[ 2 ];

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

            // since num === 2
            let sliX = container.clientWidth / 2;
            let sliY = container.clientHeight / 2;

            renderer.setViewport( sliX / 2, 0, sliX, sliY );
            renderer.render( scene, camerasB[0] );
            pointsOnScreen += renderer.info.render.points;

            renderer.setViewport( 0, sliY, sliX, sliY );
            camerasB[ 1 ].position.copy( camerasB[ 0 ].position );
            camerasB[ 1 ].quaternion.copy( camerasB[ 0 ].quaternion );
            camerasB[ 1 ].layers.enable( 0 );
            camerasB[ 1 ].layers.disable( 1 );
            renderer.render( scene, camerasB[ 1 ] );
            pointsOnScreen += renderer.info.render.points;

            renderer.setViewport( sliX, sliY, sliX, sliY );
            camerasB[ 2 ].position.copy( camerasB[ 0 ].position );
            camerasB[ 2 ].quaternion.copy( camerasB[ 0 ].quaternion );
            camerasB[ 2 ].layers.enable( 1 );
            camerasB[ 2 ].layers.disable( 0 );
            renderer.render( scene, camerasB[ 2 ] );
            pointsOnScreen += renderer.info.render.points;

        }

    }


    // GPU picking
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
        let pixelBuffer = new Uint8Array( 4 );
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

        // // resize
        window.addEventListener( 'resize', onWindowResize, false );

        // // keys
        document.addEventListener( 'keyup', onKeyUp, false);
        document.addEventListener( 'keydown', onKeyDown, false );

        // // screen buttons
        buttonFS.addEventListener( 'click', function () {

            trackScreenLay( "Full", null, null );

            flagFullScreen = true;
            flagGrid = false;
            controlsA.enabled = true;
            controlsB.enabled = false;

        }, false );

        buttonGS.addEventListener( 'click', function () {

            trackScreenLay( "Grid", null, null );

            flagFullScreen = false;
            flagGrid = true;
            controlsA.enabled = false;
            controlsB.enabled = true;

        }, false );

        // // coords buttons
        buttonKAR.addEventListener( 'click', toKartesian, false );
        buttonCYL.addEventListener( 'click', toCylindrical, false );
        buttonSPH.addEventListener( 'click', toSpherical, false );

        // draw buttons
        buttonSYNC.addEventListener( 'click', function () {

            trackDrawRangeStyle( "Sync", null, null );

            flagSync = true;
            flagAsync = false;
            syncRangeArea.style.display = 'block';
            asyncRangeArea.style.display = 'none';

        }, false );

        buttonASYNC.addEventListener( 'click', function () {

            trackDrawRangeStyle( "Async", null, null );

            flagSync = false;
            flagAsync = true;
            syncRangeArea.style.display = 'none';
            asyncRangeArea.style.display = 'block';

        }, false );

        // reset, pause buttons
        buttonRESET.addEventListener( 'click', function () {

            trackResetWay = "byButton";
            reset();

        }, false );

        buttonPAUSE.addEventListener( 'click', function () {

            trackPauseWay = "byButton";
            pause();

        }, false );

    }


    function toKartesian() {

        trackCoordSys( "Kartesian", null, timer );

        if ( ( currCoordSys !== 0 ) && ( timer === 0 ) ) {

            prevCoordSys = currCoordSys;
            timer = duration;
            currCoordSys = 0;

        }

    }

    function toCylindrical() {

        trackCoordSys( "Cylindrical", null, timer );

        if ( ( currCoordSys !== 1 ) && ( timer === 0 ) ) {

            prevCoordSys = currCoordSys;
            timer = duration;
            currCoordSys = 1;

        }

    }

    function toSpherical() {

        trackCoordSys( "Spherical", null, timer );

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

            camerasB[ c ].aspect = container.clientWidth / container.clientHeight;
            camerasB[ c ].updateProjectionMatrix();

        }

        // update the size of the renderer AND of the canvas
        renderer.setSize( container.clientWidth, container.clientHeight );
        renderer.setPixelRatio( window.devicePixelRatio );

    }


    function onKeyUp(e) {

        let ec = e.code;

        if ( ec === 'Space' ) {

            trackPauseWay = "byKey";
            pause();

        }

        if ( ec === 'KeyR' ) {

            trackResetWay = "byKey";
            reset();

        }

        // // for GPU picking...not enabled
        // if ( ec === 'KeyP' ) {
        //     picIt();
        // }

    }


    function onKeyDown(e) {

        let ec = e.code;

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

        window.snowp_in_fun( 'trackStructEvent',
                             'Interaction',
                             'PR',
                             'Reset',
                             trackResetWay );

        cloud.quaternion.copy( initCloudQuaternion );
        controlsA.reset();
        controlsB.reset();

    }


    function pause() {

        if ( flagPause ) {

            window.snowp_in_fun( 'trackStructEvent',
                                 'Interaction',
                                 'PR',
                                 'Pause',
                                 trackPauseWay );

            flagPause = false;
            dRotCloud = 0;

        } else {

            window.snowp_in_fun( 'trackStructEvent',
                                 'Interaction',
                                 'PR',
                                 'unPause',
                                 trackPauseWay );

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

        // SYNC range
        const syncRange = new JSR( ['#sync-from', '#sync-to'], {

            sliders: 2,
            values: [ syncFrom[0], syncTo[0] ],
            labels: {
                minMax: false,
                formatter: (value) => {
                    return value.toString() + '%';
                }
            }

        } );

        syncRange.addEventListener( 'update', (input, value) => {

            if ( input.id == "sync-from" ) {

                syncFrom[ 0 ] = value;
                syncFrom[ 1 ] = value;

            }

            if ( input.id == "sync-to" ) {

                syncTo[ 0 ] = value;
                syncTo[ 1 ] = value;

            }

        } );

        // RED range
        const redRange = new JSR( ['#red-from', '#red-to'], {

            sliders: 2,
            values: [ asyncFrom[0], asyncTo[0] ],
            labels: {
                minMax: false,
                formatter: (value) => {
                    return value.toString() + '%';
                }
            }

        } );

        redRange.addEventListener( 'update', (input, value) => {

            if ( input.id == "red-from" ) {

                asyncFrom[ 0 ] = value;

            }
            if ( input.id == "red-to" ) {

                asyncTo[ 0 ] = value;

            }

        } );

        // BLUE range
        const blueRange = new JSR( ['#blue-from', '#blue-to'], {

            sliders: 2,
            values: [ asyncFrom[1], asyncTo[1] ],
            labels: {
                minMax: false,
                formatter: (value) => {
                    return value.toString() + '%';
                }
            }

        } );

        blueRange.addEventListener( 'update', (input, value) => {

            if ( input.id == "blue-from" ) {

                asyncFrom[ 1 ] = value;

            }

            if ( input.id == "blue-to" ) {

                asyncTo[ 1 ] = value;

            }

        } );

    }

    let ADAx = {};

    ADAx.PointStats = function() {

        let pointStatsContainer = document.createElement( 'div' );
        pointStatsContainer.style.cssText = 'width:100%;opacity:0.9;cursor:default';

        let psDiv = document.createElement( 'div' );
        psDiv.style.cssText = 'text-align:center;';
        pointStatsContainer.appendChild( psDiv );

        let psTexts = [];
        let nLines = 2;

        for ( let i = 0; i < nLines; i++ ) {

	    psTexts[ i ] = document.createElement( 'div' );
	    psTexts[ i ].style.cssText = 'color:#00dddd;background:transparent;font-family:monospace;font-size:9px;font-weight:bold;line-height:15px';
	    psDiv.appendChild( psTexts[ i ] );
	    psTexts[ i ].innerHTML= '-';

        }

        let lastTime = Date.now();

        return {

	    domElement: pointStatsContainer,

	    update: function() {

	        // refresh only 30time per second
	        if ( Date.now() - lastTime < 1000/30 ) {
                    return;
                }

	        lastTime = Date.now();

	        let line = 0;
	        psTexts[ line++ ].textContent = "Points: " + pointsOnScreen;
                psTexts[ line++ ].textContent = picPrint;

	    }

        };

    };


    ADAx.Appendix = function() {

        let appendixContainer = document.createElement( 'div' );
        appendixContainer.style.cssText = 'width:auto;opacity:0.7;cursor:default';

        let apxDiv = document.createElement( 'div' );
        apxDiv.style.cssText = 'padding:0 0 0 0;text-align:left;background:transparent;';
        appendixContainer.appendChild( apxDiv );

        // let apxTextA = document.createElement( 'div' );
        // apxTextA.style.cssText = 'color:#f00;font-family:monospace,sans-serif;font-size:1vmin;font-weight:bold;line-height:1.1vmin';
        // apxTextA.innerHTML = '';
        // apxDiv.appendChild( apxTextA );

        let apxTextsA = [];
        let nLines = 9;  //**
        for( let i = 0; i < nLines; i++ ){

	    apxTextsA[ i ] = document.createElement( 'div' );
	    apxTextsA[ i ].style.cssText = 'color:#00dddd;background:transparent;font-family:sans-serif;font-size:1.2vmin;font-weight:normal;line-height:1.3vmin;white-space:nowrap;';
	    apxDiv.appendChild( apxTextsA[ i ] );
	    apxTextsA[ i ].innerHTML = '-';

        }

        let i = 0;
        apxTextsA[ i++ ].textContent = "====== Controls ======";
        apxTextsA[ i++ ].textContent = "A + LeftMouse: Orbiting";
        apxTextsA[ i++ ].textContent = "S + LeftMouse: Zooming";
        apxTextsA[ i++ ].textContent = "D + LeftMouse: Panning";
        apxTextsA[ i++ ].textContent = "=== FullScreen ONLY ======";
        apxTextsA[ i++ ].textContent = "Shift + 1 / 1: Hide/Show Red";
        apxTextsA[ i++ ].textContent = "Shift + 2 / 2: Hide/Show Blue";
        apxTextsA[ i++ ].textContent = "";
        apxTextsA[ i++ ].textContent = "======= RECOMMENDATIONS =======";

        // ---------------
        // RECOMMENDATIONS
        // ---------------
        let recDivs = [];
        let recLinks = [];
        for( let r = 0; r < numRECS; r++ ){

            recDivs[ r ] = document.createElement( 'div' );
	    recLinks[ r ] = document.createElement( 'a' );

	    apxDiv.appendChild( recDivs[ r ] );
            recDivs[ r ].appendChild( recLinks[ r ] );

            recLinks[ r ].innerHTML = recs[ r ][ "rec_title" ];
	    recLinks[ r ].setAttribute( 'title', recs[ r ][ "rec_title" ] );
            recLinks[ r ].setAttribute( 'href', recs[ r ][ "rec_path" ] );
            recLinks[ r ].style.cssText = 'color:#9965eb;font-family:sans-serif;font-size:1.6vmin;font-weight:bold;line-height:1.7vmin;white-space:nowrap;';

        }

        return { domElement: appendixContainer };

    };

};

export { myfun };
