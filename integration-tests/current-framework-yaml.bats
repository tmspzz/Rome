#!/usr/bin/env bats

setup() {
  
  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  git clone https://github.com/Alamofire/Alamofire.git
  cd Alamofire
  git checkout 4.7.3

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
  printf "[default]\n region = us-east-1" >> ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

  IOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/iOS/Alamofire.framework/Alamofire))
  TVOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/tvOS/Alamofire.framework/Alamofire))
  WATCHOS_DWARFDUMP_OUT=($(dwarfdump -u Carthage/Build/watchOS/Alamofire.framework/Alamofire))

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

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  ls
  echo `pwd`

  run rome upload --cache-prefix travis --no-skip-current

  [ "$status" -eq 0 ]

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../../_rome_bkp
  fi

  # Version file
  # [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-4.7.3" ]
  # [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-4.7.3" ]

  # macOS - No bitecode, No bcsymbolmap
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.framework-4.7.3.zip" ]
  [ ! -f "minio-buckets/rome/travis/Alamofire/Mac/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework-4.7.3.zip" ]
  [ ! -f "rome-local-cache/travis/Alamofire/Mac/Alamofire.framework.dSYM-4.7.3.zip" ]

  # iOS
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARMV7_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/iOS/${IOS_ARM64_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]

  # tvOS
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/tvOS/${TVOS_ARM64_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]

  # watchOS
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "minio-buckets/rome/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/Alamofire.framework.dSYM-4.7.3.zip" ]
  [ -f "rome-local-cache/travis/Alamofire/watchOS/${WATCHOS_ARMV7K_DWARF_UUID}.bcsymbolmap-4.7.3.zip" ]
  
  #save the local cache for later

  rm -rf ../../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../../_rome-local-cache_bkp

}

@test "rome downloads all artifacts for current framework skipping local cache (dynamic, yaml)" {

  if [ -d "../../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --cache-prefix travis --skip-local-cache --no-skip-current

  [ "$status" -eq 0 ]

  # Version file
  # [ -f "Carthage/Build/.Alamofire.version" ]

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

@test "rome downloads all artifacts for current framework from the local cache (dynamic, yaml)" {


  if [ -d "../../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --cache-prefix travis --no-skip-current

  [ "$status" -eq 0 ]

  # Version file
  # [ -e "Carthage/Build/.Alamofire.version" ]

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