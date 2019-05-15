#!/usr/bin/env bats

setup() {
  
  export ALAMOFIRE_VERSION="4.7.3"
  export RESULT_VERSION="4.0.0"

  cd $BATS_TMPDIR

  rm -rf Rome-Tests

  mkdir Rome-Tests && cd Rome-Tests

  if [ "$BATS_TEST_NUMBER" -eq 1 ]; then
    printf "github \"Alamofire/Alamofire\" == ${ALAMOFIRE_VERSION}\n" > Cartfile
    printf "github \"antitypical/Result\" == ${RESULT_VERSION}\n" >> Cartfile

    # carthage bootstrap --cache-builds --no-use-binaries
    
    rm -rf ../_Carthage_build_bkp
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

myfunc() {
   rome upload --concurrently --skip-local-cahe --cache-prefix travis > out.txt
 }

@test "rome uploads all artifacts (dynamic, yml)" {
  echo "# $(run rome upload --concurrently --skip-local-cache --cache-prefix travis)" >&3
  run myfunc
  echo "# $(ls)" >&3
  echo "# output of file: $(cat out.txt)" >&3
  [ 1 == 2 ]

  # Version file
  [ -f "minio-buckets/rome/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "rome-local-cache/travis/Alamofire/.Alamofire.version-${ALAMOFIRE_VERSION}" ]
  [ -f "minio-buckets/rome/travis/Result/.Result.version-4.0.0" ]
  [ -f "rome-local-cache/travis/Result/.Result.version-4.0.0" ]
  
  #save the local cache for later

  rm -rf ../_rome-local-cache_bkp
  cp -R rome-local-cache/ ../_rome-local-cache_bkp

}