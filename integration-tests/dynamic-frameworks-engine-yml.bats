#!/usr/bin/env bats

setup() {
  
  export ALAMOFIRE_VERSION="4.7.3"
  export RESULT_VERSION="4.0.0"

  rm -rf $BATS_TMPDIR/Rome-Tests
  mkdir -p $BATS_TMPDIR/Rome-Tests
  cp engine.sh $BATS_TMPDIR/Rome-Tests/
  cd $BATS_TMPDIR/Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"Alamofire/Alamofire\" == ${ALAMOFIRE_VERSION}\n" > Cartfile
    printf "github \"antitypical/Result\" == ${RESULT_VERSION}\n" >> Cartfile

    # TODO: uncomment following line
    #carthage bootstrap --cache-builds --no-use-binaries
    
    rm -rf ../_Carthage_build_bkp
    # TODO: remove this line that was only for testing purposes
    cp -R /Users/balestrapatrick/Desktop/integration-test/ .
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
  
  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  engine: engine.sh
ignoreMap:
  - Alamofire:
    - name: Alamofire
      platforms: [Mac]
EOF

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/iOS/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/tvOS/Alamofire.framework/Alamofire))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/watchOS/Alamofire.framework/Alamofire))

  export ALAMOFIRE_IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export ALAMOFIRE_IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export ALAMOFIRE_TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/iOS/Result.framework/Result))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/tvOS/Result.framework/Result))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/watchOS/Result.framework/Result))

  export RESULT_IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export RESULT_IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export RESULT_TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export RESULT_WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}
  
  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3
  
}

teardown() {
  cd $BATS_TEST_DIRNAME
}

@test "rome uploads all artifacts with engine (dynamic, yml)" {

  # TODO: remove custom path and use `rome`
  run /Users/balestrapatrick/GitHub/Rome/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/bin/rome upload --concurrently --cache-prefix travis
  
  [ "$status" -eq 0 ]

  # Version file
  [ -f "server-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "server-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  [ -f "rome-local-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  
  # macOS - No bitecode, No bcsymbolmap

  # macOS - Alamofire
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]

  # macOS - Result
  [ -f "server-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/Mac/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/Mac/Result.framework.dSYM-${RESULT_VERSION}.zip" ]

  # iOS - Alamofire
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  
  # iOS - Result
  [ -f "server-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/iOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]

  # tvOS - Alamofire
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]

  # tvOS - Result
  [ -f "server-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/tvOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]

  # watchOS
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]

  [ -f "server-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/watchOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "server-cache/travis/Result/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  
  # save the server cache for later
  rm -rf ../_server-cache_bkp
  cp -R server-cache/ ../_server-cache_bkp

  # save the local cache for later
  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp
}

@test "rome downloads all artifacts with engine skipping local cache (dynamic, yml)" {

  # restore server cache
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../_server-cache_bkp server-cache/
    # echo "# $(ls)" >&3
  fi

  # restore local cache (even though it will be skipped, we want it to be there to simulate a real scenario)
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../_rome-local-cache_bkp rome-local-cache/
  fi

  rm -rf Carthage/Build
  # TODO: remove custom path and use `rome`
  run /Users/balestrapatrick/GitHub/Rome/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/bin/rome download --concurrently --skip-local-cache --cache-prefix travis
  
  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]
  [ -f "Carthage/Build/.Result.version" ]

  # macOS - No bitcode, No bcsymbolmap
  # macOS - Alamofire
  [ ! -d "Carthage/Build/Mac/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Mac/Alamofire.framework.dSYM" ]

  # macOS - Result
  [ -d "Carthage/Build/Mac/Result.framework" ]
  [ -d "Carthage/Build/Mac/Result.framework.dSYM" ]

  # iOS - Alamofire
  [ -d "Carthage/Build/iOS/Alamofire.framework" ]
  [ -d "Carthage/Build/iOS/Alamofire.framework.dSYM" ]
  [ -e "Carthage/Build/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -e "Carthage/Build/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # iOS - Result
  [ -d "Carthage/Build/iOS/Result.framework" ]
  [ -d "Carthage/Build/iOS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS - Alamofire
  [ -d "Carthage/Build/tvOS/Alamofire.framework" ]
  [ -d "Carthage/Build/tvOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]
  
  # tvOS - Result
  [ -d "Carthage/Build/tvOS/Result.framework" ]
  [ -d "Carthage/Build/tvOS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Alamofire
  [ -d "Carthage/Build/watchOS/Alamofire.framework" ]
  [ -d "Carthage/Build/watchOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Result
  [ -d "Carthage/Build/watchOS/Result.framework" ]
  [ -d "Carthage/Build/watchOS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all artifacts with engine from the local cache (dynamic, yml)" {

  # restore local cache
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../_rome-local-cache_bkp rome-local-cache/
  fi
  
  rm -rf Carthage/Build
  # TODO: remove custom path and use `rome`
  run /Users/balestrapatrick/GitHub/Rome/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/bin/rome download --concurrently --cache-prefix travis

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]
  [ -f "Carthage/Build/.Result.version" ]

  # macOS - No bitecode, No bcsymbolmap
  # macOS - Alamofire
  [ ! -d "Carthage/Build/Mac/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Mac/Alamofire.framework.dSYM" ]

  # macOS - Result
  [ -d "Carthage/Build/Mac/Result.framework" ]
  [ -d "Carthage/Build/Mac/Result.framework.dSYM" ]

  # iOS - Alamofire
  [ -d "Carthage/Build/iOS/Alamofire.framework" ]
  [ -d "Carthage/Build/iOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # iOS - Result
  [ -d "Carthage/Build/iOS/Result.framework" ]
  [ -d "Carthage/Build/iOS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS - Alamofire
  [ -d "Carthage/Build/tvOS/Alamofire.framework" ]
  [ -d "Carthage/Build/tvOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS - Result
  [ -d "Carthage/Build/tvOS/Result.framework" ]
  [ -d "Carthage/Build/tvOS/Result.framework.dSYM" ]
  [ -e "Carthage/Build/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Alamofire
  [ -d "Carthage/Build/watchOS/Alamofire.framework" ]
  [ -d "Carthage/Build/watchOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Result
  [ -d "Carthage/Build/watchOS/Result.framework" ]
  [ -d "Carthage/Build/watchOS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}