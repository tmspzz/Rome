#!/usr/bin/env bats

setup() {
  
  export ALAMOFIRE_VERSION="4.8.2"
  export RESULT_VERSION="4.0.0"

  rm -rf $BATS_TMPDIR/Rome-Tests

  mkdir -p $BATS_TMPDIR/Rome-Tests

  cp integration-tests/engine.sh $BATS_TMPDIR/Rome-Tests/

  cd $BATS_TMPDIR/Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"Alamofire/Alamofire\" == ${ALAMOFIRE_VERSION}\n" > Cartfile
    printf "github \"antitypical/Result\" == ${RESULT_VERSION}\n" >> Cartfile

    carthage bootstrap --cache-builds --no-use-binaries --use-xcframeworks
    
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
  
  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  engine: engine.sh
ignoreMap:
  - Alamofire:
    - name: Alamofire
      platforms: [Mac]
EOF

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework/Alamofire))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework/Alamofire))

  export ALAMOFIRE_IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export ALAMOFIRE_IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export ALAMOFIRE_TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Result.xcframework/ios-arm64_armv7/Result.framework/Result))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Result.xcframework/tvos-arm64/Result.framework/Result))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/Result.framework/Result))

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

  run rome upload --concurrently --cache-prefix travis --use-xcframeworks
  
  [ "$status" -eq 0 ]

  # Version file
  [ -f "server-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "server-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  [ -f "rome-local-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]

  # macOS - Alamofire
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # macOS - Result
  [ -f "server-cache/travis/Result/Mac/Result.xcframework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/Mac/Result.xcframework-${RESULT_VERSION}.zip" ]

  # iOS - Alamofire
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  
  # iOS - Result
  [ -f "server-cache/travis/Result/iOS/Result.xcframework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/Result.xcframework-${RESULT_VERSION}.zip" ]

  # tvOS - Alamofire
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # tvOS - Result
  [ -f "server-cache/travis/Result/tvOS/Result.xcframework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/Result.xcframework-${RESULT_VERSION}.zip" ]

  # watchOS - Alamofire
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # watchOS - Result
  [ -f "server-cache/travis/Result/watchOS/Result.xcframework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/Result.xcframework-${RESULT_VERSION}.zip" ]
  
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
  fi

  # restore local cache (even though it will be skipped, we want it to be there to simulate a real scenario)
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../_rome-local-cache_bkp rome-local-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --skip-local-cache --cache-prefix travis --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]
  [ -f "Carthage/Build/.Result.version" ]

  # macOS - Alamofire
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # macOS - Result
  [ -d "Carthage/Build/Result.xcframework/macos-x86_64/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/macos-x86_64/dSYMs/Result.framework.dSYM" ]

  # iOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # iOS - Result
  [ -d "Carthage/Build/Result.xcframework/ios-arm64_armv7/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/ios-arm64_armv7/dSYMs/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/ios-arm64_armv7/BCSymbolMaps/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/Result.xcframework/ios-arm64_armv7/BCSymbolMaps/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]
  
  # tvOS - Result
  [ -d "Carthage/Build/Result.xcframework/tvos-arm64/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/tvos-arm64/dSYMS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/tvos-arm64/BCSymbolMaps/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Result
  [ -d "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/dSYMs/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all artifacts with engine from the local cache (dynamic, yml)" {

  # restore local cache
  if [ -d "../_server-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../_rome-local-cache_bkp rome-local-cache/
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]
  [ -f "Carthage/Build/.Result.version" ]

  # macOS - Alamofire
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # macOS - Result
  [ -d "Carthage/Build/Result.xcframework/macos-x86_64/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/macos-x86_64/dSYMs/Result.framework.dSYM" ]

  # iOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # iOS - Result
  [ -d "Carthage/Build/Result.xcframework/ios-arm64_armv7/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/ios-arm64_armv7/dSYMs/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/ios-arm64_armv7/BCSymbolMaps/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/Result.xcframework/ios-arm64_armv7/BCSymbolMaps/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]
  
  # tvOS - Result
  [ -d "Carthage/Build/Result.xcframework/tvos-arm64/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/tvos-arm64/dSYMS/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/tvos-arm64/BCSymbolMaps/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Alamofire
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]

  # watchOS - Result
  [ -d "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/Result.framework" ]
  [ -d "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/dSYMs/Result.framework.dSYM" ]
  [ -f "Carthage/Build/Result.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}