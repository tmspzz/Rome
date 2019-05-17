#!/usr/bin/env bats

setup() {

  export FRAMEWORK_VERSION=0.4.0
  export FRAMEWORK_REPO_NAME=swift-tagged
  export FRAMEWORK_ARTIFACT_NAME=Tagged

  rm -rf $BATS_TMPDIR/Rome-Tests

  mkdir -p $BATS_TMPDIR/Rome-Tests

  cp integration-tests/engine.sh $BATS_TMPDIR/Rome-Tests/

  cd $BATS_TMPDIR/Rome-Tests

  git clone https://github.com/pointfreeco/swift-tagged.git
  cd swift-tagged
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
  - swift-tagged:
    - name: Tagged
      platforms: [Mac]
currentMap:
  - swift-tagged:
    - name: Tagged
EOF

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework/${FRAMEWORK_ARTIFACT_NAME}))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework/${FRAMEWORK_ARTIFACT_NAME}))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework/${FRAMEWORK_ARTIFACT_NAME}))

  export IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}

  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3

}

teardown() {
  cd $BATS_TEST_DIRNAME
}

@test "rome uploads all named artifacts for current framework with engine (dynamic, yaml)" {
  
  run rome upload --concurrently --cache-prefix travis ${FRAMEWORK_REPO_NAME}

  [ "$status" -eq 0 ]

  # Version file
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/.${FRAMEWORK_REPO_NAME}.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/.${FRAMEWORK_REPO_NAME}.version-${FRAMEWORK_VERSION}" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]

  # iOS
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # tvOS
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # watchOS
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "server-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/${FRAMEWORK_REPO_NAME}/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${FRAMEWORK_VERSION}.zip" ]

  # save the server cache for later
  rm -rf ../../_server-cache_bkp
  cp -R server-cache/ ../../_server-cache_bkp

  # save the local cache for later
  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp
}

@test "rome downloads all named artifacts for current framework with engine skipping local cache (dynamic, yaml)" {

  # restore server cache
  if [ -d "../../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../../_server-cache_bkp server-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache ${FRAMEWORK_REPO_NAME} 

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.${FRAMEWORK_REPO_NAME}.version" ]

  # macOS - No bitcode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all named artifacts for current framework with engine from the local cache (dynamic, yaml)" {

  # restore local cache
  if [ -d "../../_rome-local-cache_bkp" ]; then
    echo "# Local cache restored" >&3
    cp -R ../../_rome-local-cache_bkp rome-local-cache/
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis ${FRAMEWORK_REPO_NAME}

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.${FRAMEWORK_REPO_NAME}.version" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads named artifacts for current framework with engine skipping local cache (dynamic, yaml)" {

  # restore server cache
  if [ -d "../../_server-cache_bkp" ]; then
    echo "# Server cache restored" >&3
    cp -R ../../_server-cache_bkp server-cache/
  fi

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache ${FRAMEWORK_REPO_NAME}

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.${FRAMEWORK_REPO_NAME}.version" ]

  # macOS - No bitcode, No bcsymbolmap
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ ! -d "Carthage/Build/Mac/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/iOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/tvOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework" ]
  [ -d "Carthage/Build/watchOS/${FRAMEWORK_ARTIFACT_NAME}.framework.dSYM" ]
  [ -f "Carthage/Build/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}
