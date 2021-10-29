#!/usr/bin/env bats

setup() {

  export FRAMEWORK_VERSION=4.8.2

  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"Alamofire/Alamofire\" == %s \n" ${FRAMEWORK_VERSION} > Cartfile
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

  printf "[Cache]\n S3-Bucket = rome\n local = rome-local-cache" > Romefile
  mkdir -p ~/.aws
  printf "[default]\n region = us-east-1" > ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/BuildAlamofire.xcframework/tvos-arm64/Alamofire.framework/Alamofire))
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


@test "rome uploads all artifacts (dynamic, ini)" {

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  run rome upload --concurrently --cache-prefix travis --use-xcframeworks

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../_rome_bkp
  fi

  [ "$status" -eq 0 ]

  # Version file
  [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${FRAMEWORK_VERSION}" ]

  # macOS
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # iOS
  [ -f "minio-buckets/travis/Alamofire/iOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # tvOS
  [ -f "minio-buckets/travis/Alamofire/tvOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]

  # watchOS
  [ -f "minio-buckets/travis/Alamofire/watchOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.xcframework-${ALAMOFIRE_VERSION}.zip" ]
  
  #save the local cache for later

  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp

}

@test "rome downloads all artifacts skipping local cache (dynamic, ini)" {

  if [ -d "../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Alamofire.version" ]

  # macOS
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}

@test "rome downloads all artifacts from the local cache (dynamic, ini)" {
  
  if [ -d "../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --use-xcframeworks

  [ "$status" -eq 0 ]

  # Version file
  [ -e "Carthage/Build/.Alamofire.version" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/Alamofire.framework" ]
  [ ! -d "Carthage/Build/Alamofire.xcframework/macos-arm64_x86_64/dSYMs/Alamofire.framework.dSYM" ]

  # iOS
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/dSYMs/Alamofire.framework.dSYM" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap" ]
  [ -e "Carthage/Build/Alamofire.xcframework/ios-arm64_armv7/BCSymbolMaps/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # tvOS
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/tvos-arm64/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/tvos-arm64/BCSymbolMaps/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap" ]

  # watchOS
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/Alamofire.framework" ]
  [ -d "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/dSYMs/Alamofire.framework.dSYM" ]
  [ -f "Carthage/Build/Alamofire.xcframework/watchos-arm64_32_armv7k/BCSymbolMaps/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap" ]
}
