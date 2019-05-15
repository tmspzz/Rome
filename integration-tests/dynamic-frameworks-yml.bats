#!/usr/bin/env bats

setup() {
  
  export ALAMOFIRE_VERSION="4.8.2"
  export RESULT_VERSION="4.1.0"

  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"Alamofire/Alamofire\" == %s \n" ${ALAMOFIRE_VERSION} > Cartfile
    printf "github \"antitypical/Result\" == %s \n"  ${RESULT_VERSION} >> Cartfile

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
  
  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  s3Bucket: rome
ignoreMap:
  - Alamofire:
    - name: Alamofire
      platforms: [Mac]
EOF
  mkdir -p ~/.aws
  printf "[default]\n region = us-east-1" >> ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

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


@test "rome uploads all artifacts (dynamic, yml)" {

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  run rome upload --concurrently --cache-prefix travis

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../_rome_bkp
  fi

  [ "$status" -eq 0 ]

  # Version file
  [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "minio-buckets/rome/travis/Result/.Result.version-${RESULT_VERSION}" ]
  [ -f "rome-local-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]

  # macOS - No bitecode, No bcsymbolmap

  # macOS - Alamofire
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]

  # macOS - Result
  [ -f "minio-buckets/rome/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/Mac/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/Mac/Result.framework.dSYM-${RESULT_VERSION}.zip" ]

  # iOS - Alamofire
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${ALAMOFIRE_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  
  # iOS - Result
  [ -f "minio-buckets/rome/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/iOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/${RESULT_IOS_ARMV7_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/iOS/${RESULT_IOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]

  # tvOS - Alamofire
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/${ALAMOFIRE_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]

  # tvOS - Result
  [ -f "minio-buckets/rome/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/tvOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/tvOS/${RESULT_TVOS_ARM64_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]

  # watchOS
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-${ALAMOFIRE_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/${ALAMOFIRE_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${ALAMOFIRE_VERSION}.zip" ]

  [ -f "minio-buckets/rome/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/watchOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "minio-buckets/rome/travis/Result/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/Result.framework.dSYM-${RESULT_VERSION}.zip" ]
  [ -f "rome-local-cache/travis/Result/watchOS/${RESULT_WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-${RESULT_VERSION}.zip" ]
  
  #save the local cache for later

  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp

}

@test "rome downloads all artifacts skipping local cache (dynamic, yml)" {

  if [ -d "../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis --skip-local-cache

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

@test "rome downloads all artifacts from the local cache (dynamic, yml)" {
  
  if [ -d "../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis

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
