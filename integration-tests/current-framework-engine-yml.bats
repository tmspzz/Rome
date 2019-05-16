#!/usr/bin/env bats

setup() {

  export FRAMEWORK_VERSION=4.8.2
  
  rm -rf $BATS_TMPDIR/Rome-Tests

  mkdir -p $BATS_TMPDIR/Rome-Tests

  cp engine.sh $BATS_TMPDIR/Rome-Tests/
  
  cd $BATS_TMPDIR/Rome-Tests
  
  git clone https://github.com/Alamofire/Alamofire.git
  cd Alamofire
  git checkout ${FRAMEWORK_VERSION}

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then

    carthage build --no-use-binaries --no-skip-current --cache-builds

    rm -rf ../../_Carthage_build_bkp
    cp -R Carthage/Build/ ../../_Carthage_build_bkp

  else
    mkdir -p Carthage/Build
    cp -R ../../_Carthage_build_bkp/ Carthage/Build
  fi

  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  engine: ../engine.sh
ignoreMap:
  - Alamofire:
    - name: Alamofire
      platforms: [Mac]
currentMap:
  - Alamofire:
    - name: Alamofire
EOF

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/iOS/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/tvOS/Alamofire.framework/Alamofire))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/watchOS/Alamofire.framework/Alamofire))

  export IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}
  
  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3

}

teardown() {
  cd $BATS_TEST_DIRNAME
}

@test "rome uploads all artifacts for current framework with engine (dynamic, yaml)" {

  run rome upload --concurrently --cache-prefix travis --no-skip-current

  [ "$status" -eq 0 ]
  
  # Version file
  [ -f "server-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]

  # iOS
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # tvOS
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # watchOS
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  
  # save the server cache for later
  rm -rf ../../_server-cache_bkp
  cp -R server-cache/ ../../_server-cache_bkp

  # save the local cache for later
  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp
}

@test "rome downloads all artifacts for current framework with engine skipping local cache (dynamic, yaml)" {

  # restore server cache
  if [ -d "../../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../../_server-cache_bkp server-cache/
  fi

  # restore local cache (even though it will be skipped, we want it to be there to simulate a real scenario)
  if [ -d "../../_rome-local-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../../_rome-local-cache_bkp rome-local-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --no-skip-current

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS - No bitcode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Mac/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/Alamofire.framework" ]
  [ -d "Carthage/Build/iOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/Alamofire.framework" ]
  [ -d "Carthage/Build/tvOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/Alamofire.framework" ]
  [ -d "Carthage/Build/watchOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all artifacts for current framework with engine from the local cache (dynamic, yaml)" {

  # restore local cache
  if [ -d "../../_rome-local-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../../_rome-local-cache_bkp rome-local-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --no-skip-current

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Mac/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/Alamofire.framework" ]
  [ -d "Carthage/Build/iOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/Alamofire.framework" ]
  [ -d "Carthage/Build/tvOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/Alamofire.framework" ]
  [ -d "Carthage/Build/watchOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome uploads named artifacts for current framework with engine (dynamic, yaml)" {
  
  run rome upload --concurrently --cache-prefix travis --no-skip-current Alamofire

  [ "$status" -eq 0 ]

  # Version file
  [ -f "server-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "server-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]

  # iOS
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # tvOS
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # watchOS
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  
  # save the local cache for later
  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp
}

@test "rome downloads named artifacts for current framework with engine skipping local cache (dynamic, yaml)" {

  # restore server cache
  if [ -d "../../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../../_server-cache_bkp server-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --no-skip-current Alamofire

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS - No bitcode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Mac/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/Alamofire.framework" ]
  [ -d "Carthage/Build/iOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/Alamofire.framework" ]
  [ -d "Carthage/Build/tvOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/Alamofire.framework" ]
  [ -d "Carthage/Build/watchOS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}