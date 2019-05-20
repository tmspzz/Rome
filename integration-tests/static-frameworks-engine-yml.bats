#!/usr/bin/env bats

setup() {
  
  export STATICFIRE_COMMIT="ada9256edf4b6dfb63bd1f6d13441b1ab7fd6ffd"
  export RESULT_VERSION="4.1.0"

  rm -rf $BATS_TMPDIR/Rome-Tests

  mkdir -p $BATS_TMPDIR/Rome-Tests

  cp integration-tests/engine.sh $BATS_TMPDIR/Rome-Tests/

  cd $BATS_TMPDIR/Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"blender/Staticfire\" \"master\"\n" > Cartfile
    printf "github \"antitypical/Result\" == ${RESULT_VERSION}\n" >> Cartfile
    
    carthage bootstrap --cache-builds --no-use-binaries
    
    rm -rf ../_Carthage_build_bkp
    cp -R Carthage/Build/ ../_Carthage_build_bkp

    rm -f ../_Cartfile_bkp
    rm -f ../_Cartfile.resolved_bkp
    cp Cartfile ../_Cartfile_bkp
    cp Cartfile.resolved ../_Cartfile.resolved_bkp

  else 
    mkdir -p Carthage/Build
    cp -R ../_Carthage_build_bkp/ Carthage/Build
    cp ../_Cartfile_bkp Cartfile 
    cp ../_Cartfile.resolved_bkp Cartfile.resolved 
  fi

  cat >> ../Romefile.yaml << EOF
cache:
  local: rome-local-cache
  engine: engine.sh
repositoryMap:
- Staticfire:
  - name: Alamofire
    type: static
ignoreMap:
- Staticfire:
  - name: Alamofire
    type: static
    platforms: [Mac]
- Result:
  - name: Result
    platforms: [iOS,Mac,tvOS,watchOS]
EOF

  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3

}

teardown() {
  cd $BATS_TEST_DIRNAME
}


@test "rome uploads all artifacts with engine (static, yml)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

  run rome upload --concurrently --cache-prefix travis --romefile ../Romefile.yaml

  [ "$status" -eq 0 ]

  # Version file
  [ -f "server-cache/travis/Staticfire/.Staticfire.version-${STATICFIRE_COMMIT}" ]
  [ -f "rome-local-cache/travis/Staticfire/.Staticfire.version-${STATICFIRE_COMMIT}" ]
  [ ! -f "server-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  [ ! -f "rome-local-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  
  # macOS - Staticfire
  [ ! -f "server-cache/travis/Staticfire/Mac/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ ! -f "rome-local-cache/travis/Staticfire/Mac/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # macOS - Result
  [ ! -f "server-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]

  # iOS - Staticfire
  [ -f "server-cache/travis/Staticfire/iOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/iOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # iOS - Result
  [ ! -f "server-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]

  # tvOS - Staticfire
  [ -f "server-cache/travis/Staticfire/tvOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/tvOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # tvOS - Result
  [ ! -f "server-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]

  # watchOS - Staticfire
  [ -f "server-cache/travis/Staticfire/watchOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/watchOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # watchOS - Result
  [ ! -f "server-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  
  # save the server cache for later
  rm -rf ../_server-cache_bkp
  cp -R server-cache/ ../_server-cache_bkp

  # save the local cache for later
  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp
}

@test "rome downloads all artifacts with engine skipping local cache (static, yml)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

  # restore server cache
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../_server-cache_bkp server-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --romefile ../Romefile.yaml

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Staticfire.version" ]
  [ ! -f "Carthage/Build/.Result.version" ]


  # macOS - Staticfire
  [ ! -d "Carthage/Build/Mac/Static/Alamofire.framework" ]
  # macOS - Result
  [ ! -d "Carthage/Build/Mac/Result.framework" ]

  # iOS - Staticfire
  [ -d "Carthage/Build/iOS/Static/Alamofire.framework" ]
  # iOS - Result
  [ ! -d "Carthage/Build/iOS/Result.framework" ]

  # tvOS - Staticfire
  [ -d "Carthage/Build/tvOS/Static/Alamofire.framework" ]
  # tvOS - Result
  [ ! -d "Carthage/Build/tvOS/Result.framework" ]

  # watchOS - Staticfire
  [ -d "Carthage/Build/watchOS/Static/Alamofire.framework" ]
  # watchOS - Result
  [ ! -d "Carthage/Build/watchOS/Result.framework" ]

}

@test "rome downloads all artifacts with engine from the local cache (static, yml)" {
  
  # No dSYMs nor bcsymbolmaps for Static Frameworks

  # restore local cache
  if [ -d "../_rome-local-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../_rome-local-cache_bkp rome-local-cache/
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --romefile ../Romefile.yaml

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Staticfire.version" ]

  # macOS - No bitecode, No bcsymbolmap
  # macOS - Staticfire
  [ ! -d "Carthage/Build/Mac/Static/Alamofire.framework" ]
  # macoS - Result
  [ ! -d "Carthage/Build/Mac/Result.framework" ]

  # iOS - Staticfire
  [ -d "Carthage/Build/iOS/Static/Alamofire.framework" ]
  # iOS - Result
  [ ! -d "Carthage/Build/iOS/Result.framework" ]

  # tvOS - Staticfire
  [ -d "Carthage/Build/tvOS/Static/Alamofire.framework" ]
  # tvOS - Result
  [ ! -d "Carthage/Build/tvOS/Result.framework" ]

  # watchOS - Staticfire
  [ -d "Carthage/Build/watchOS/Static/Alamofire.framework" ]
  # watchOS - Result
  [ ! -d "Carthage/Build/watchOS/Result.framework" ]
}
