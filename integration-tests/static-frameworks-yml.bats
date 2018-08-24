#!/usr/bin/env bats

setup() {
  
  export STATICFIRE_COMMIT="c9cc8ccafbb715700fc350266cdb60553e282ffb"
  export RESULT_VERSION="4.0.0"

  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

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

  cat >> Romefile << EOF
cache:
  local: rome-local-cache
  s3Bucket: rome
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
  mkdir -p ~/.aws
  printf "[default]\n region = us-east-1" >> ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

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


@test "rome uploads all artifacts (static, yml)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  run rome upload --cache-prefix travis

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../_rome_bkp
  fi

  [ "$status" -eq 0 ]

  # Version file
  [ -f "minio-buckets/rome/travis/Staticfire/.Staticfire.version-${STATICFIRE_COMMIT}" ]
  [ -f "rome-local-cache/travis/Staticfire/.Staticfire.version-${STATICFIRE_COMMIT}" ]
  [ ! -f "minio-buckets/rome/travis/Result/.Result.version-${RESULT_VERSION}" ]
  [ ! -f "rome-local-cache/travis/Result/.Result.version-${RESULT_VERSION}" ]
  
  # macOS - Staticfire
  [ ! -f "minio-buckets/rome/travis/Staticfire/Mac/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ ! -f "rome-local-cache/travis/Staticfire/Mac/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # macOS - Result
  [ ! -f "minio-buckets/rome/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/Mac/Result.framework-${RESULT_VERSION}.zip" ]

  # iOS - Staticfire
  [ -f "minio-buckets/rome/travis/Staticfire/iOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/iOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # iOS - Result
  [ ! -f "minio-buckets/rome/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/iOS/Result.framework-${RESULT_VERSION}.zip" ]

  # tvOS - Staticfire
  [ -f "minio-buckets/rome/travis/Staticfire/tvOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/tvOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # tvOS - Result
  [ ! -f "minio-buckets/rome/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/tvOS/Result.framework-${RESULT_VERSION}.zip" ]

  # watchOS - Staticfire
  [ -f "minio-buckets/rome/travis/Staticfire/watchOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/watchOS/Alamofire.framework-static-${STATICFIRE_COMMIT}.zip" ]
  # watchOS - Result
  [ ! -f "minio-buckets/rome/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  [ ! -f "rome-local-cache/travis/Result/watchOS/Result.framework-${RESULT_VERSION}.zip" ]
  
  #save the local cache for later

  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp

}

@test "rome downloads all artifacts skipping local cache (static, yml)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

  if [ -d "../_rome_bkp" ]; then
    echo "# Minio bucket restored" >&3
    cp -R ../_rome_bkp/ minio-buckets/rome
  fi

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  rm -rf Carthage/Build
  run rome download --cache-prefix travis --skip-local-cache

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

@test "rome downloads all artifacts from the local cache (static, yml)" {
  
  # No dSYMs nor bcsymbolmaps for Static Frameworks

  if [ -d "../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --cache-prefix travis

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