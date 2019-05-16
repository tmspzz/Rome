#!/usr/bin/env bats

setup() {
  
  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    echo 'github "blender/Staticfire" "master"' > Cartfile
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

  printf "[Cache]\n S3-Bucket = rome\n local = rome-local-cache\n[RepositoryMap]\n Staticfire = static/Alamofire" >> Romefile
  mkdir -p ~/.aws
  printf "[default]\n region = us-east-1" >> ~/.aws/config

  # minio

  mkdir -p minio-buckets/rome

  export AWS_ACCESS_KEY_ID=Q3AM3UQ867SPQQA43P2F
  export AWS_SECRET_ACCESS_KEY=zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG
  export MINIO_ACCESS_KEY=Q3AM3UQ867SPQQA43P2F
  export MINIO_SECRET_KEY=zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG
  export AWS_ENDPOINT=http://127.0.0.1:9000 

  export COMMIT=ada9256edf4b6dfb63bd1f6d13441b1ab7fd6ffd

  echo "# BATS_TMPDIR: ${BATS_TMPDIR}" >&3

}

teardown() {
  
  if [ ! "$BATS_TEST_NUMBER" -eq 3 ]; then
    killall minio
  fi
  cd $BATS_TEST_DIRNAME
}


@test "rome uploads all artifacts (static)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

  MINIO_HTTP_TRACE=output.log minio server minio-buckets &
  sleep 4 

  run rome upload --concurrently --cache-prefix travis

  if [ -d "minio-buckets/rome" ]; then
    cp -R minio-buckets/rome/ ../_rome_bkp
  fi

  [ "$status" -eq 0 ]

  # Version file
  [ -f "minio-buckets/rome/travis/Staticfire/.Staticfire.version-${COMMIT}" ]
  [ -f "rome-local-cache/travis/Staticfire/.Staticfire.version-${COMMIT}" ]
  
  # macOS
  [ -f "minio-buckets/rome/travis/Staticfire/Mac/Alamofire.framework-static-${COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/Mac/Alamofire.framework-static-${COMMIT}.zip" ]

  # iOS
  [ -f "minio-buckets/rome/travis/Staticfire/iOS/Alamofire.framework-static-${COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/iOS/Alamofire.framework-static-${COMMIT}.zip" ]

  # tvOS
  [ -f "minio-buckets/rome/travis/Staticfire/tvOS/Alamofire.framework-static-${COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/tvOS/Alamofire.framework-static-${COMMIT}.zip" ]

  # watchOS
  [ -f "minio-buckets/rome/travis/Staticfire/watchOS/Alamofire.framework-static-${COMMIT}.zip" ]
  [ -f "rome-local-cache/travis/Staticfire/watchOS/Alamofire.framework-static-${COMMIT}.zip" ]
  
  #save the local cache for later

  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp

}

@test "rome downloads all artifacts skipping local cache (static)" {

  # No dSYMs nor bcsymbolmaps for Static Frameworks

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
  [ -f "Carthage/Build/.Staticfire.version" ]

  # macOS
  [ -d "Carthage/Build/Mac/Static/Alamofire.framework" ]

  # iOS
  [ -d "Carthage/Build/iOS/Static/Alamofire.framework" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/Static/Alamofire.framework" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/Static/Alamofire.framework" ]
}

@test "rome downloads all artifacts from the local cache (static)" {
  
  # No dSYMs nor bcsymbolmaps for Static Frameworks

  if [ -d "../_rome-local-cache_bkp" ]; then
    echo "# Rome local cache restored" >&3
    cp -R ../_rome-local-cache_bkp/ rome-local-cache
  fi
  
  rm -rf Carthage/Build
  run rome download --concurrently --cache-prefix travis

  [ "$status" -eq 0 ]

  # Version file
  [ -f "Carthage/Build/.Staticfire.version" ]

  # macOS - No bitecode, No bcsymbolmap
  [ -d "Carthage/Build/Mac/Static/Alamofire.framework" ]

  # iOS
  [ -d "Carthage/Build/iOS/Static/Alamofire.framework" ]

  # tvOS
  [ -d "Carthage/Build/tvOS/Static/Alamofire.framework" ]

  # watchOS
  [ -d "Carthage/Build/watchOS/Static/Alamofire.framework" ]
}
