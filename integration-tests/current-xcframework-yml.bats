#!/usr/bin/env bats

setup() {

  export FRAMEWORK_VERSION=4.8.2

  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  git clone https://github.com/Alamofire/Alamofire.git
  cd Alamofire
  git checkout ${FRAMEWORK_VERSION}

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then

    carthage build --no-use-binaries --no-skip-current --cache-builds --use-xcframeworks

    rm -rf ../../_Carthage_build_bkp
    cp -R Carthage/Build/ ../../_Carthage_build_bkp

  else
    mkdir -p Carthage/Build
    cp -R ../../_Carthage_build_bkp/ Carthage/Build
  fi

  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  s3Bucket: rome
ignoreMap:
  - Alamofire:
    - name: Alamofire
      platforms: [Mac]
currentMap:
  - Alamofire:
    - name: Alamofire
EOF

  mkdir -p ~/.aws
  printf "[default]\n region = us-east-1" > ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework/Alamofire))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework/Alamofire))

  export IOS_ARMV7_DWARF_UUID=${IOS_DWARFDUMP_OUT[9]}
  export IOS_ARM64_DWARF_UUID=${IOS_DWARFDUMP_OUT[13]}
  export TVOS_ARM64_DWARF_UUID=${TVOS_DWARFDUMP_OUT[5]}
  export WATCHOS_ARMV7K_DWARF_UUID=${WATCHOS_DWARFDUMP_OUT[5]}

  export AWS_ACCESS_KEY_ID=Q3AM3UQ867SPQQA43P2F
  export AWS_SECRET_ACCESS_KEY=zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG
  export MINIO_ACCESS_KEY=Q3AM3UQ867SPQQA43P2F
  export MINIO_SECRET_KEY=zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG
  export AWS_ENDPOINT=http://127.0.0.1:9000 
  
  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3

}

teardown() {
  
  if [ ! "$BATS_TEST_NUMBER" -eq 3 ]; then
    killall minio
  fi
  cd $BATS_TEST_DIRNAME
}


@test "rome uploads all artifacts for current framework (dynamic, yaml)" {

  # Test 1

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  ls
  echo `pwd`

  run rome upload --concurrently --cache-prefix travis --no-skip-current --use-xcframeworks

  [ "$status" -eq 0 ]

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../../_rome_bkp
  fi

  # Version file
  [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]

  # macOS
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]

  # iOS
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]

  # tvOS
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]

  # watchOS
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  
  #save the local cache for later

  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp

}

@test "rome downloads all artifacts for current framework skipping local cache (dynamic, yaml)" {

  # Test 2

  if [ -d "../../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --no-skip-current --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all artifacts for current framework from the local cache (dynamic, yaml)" {

  # Test 3

  if [ -d "../../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --no-skip-current --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome uploads named artifacts for current framework (dynamic, yaml)" {
  
  # Test 4

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  ls
  echo `pwd`

  run rome upload --concurrently --cache-prefix travis --no-skip-current --use-xcframeworks Alamofire 

  [ "$status" -eq 0 ]

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../../_rome_bkp
  fi

  # Version file
  [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]

  # macOS
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]

  # iOS
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${FRAMEWORK_VERSION}.zip" ]

  # tvOS
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]

  # watchOS
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.xcframework-${FRAMEWORK_VERSION}.zip" ]
  
  #save the local cache for later

  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp

}

@test "rome downloads named artifacts for current framework skipping local cache (dynamic, yaml)" {

  # Test 5

  if [ -d "../../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --no-skip-current --use-xcframeworks Alamofire

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -f "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMS/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}
