# Changelog

## [Unreleased](https://github.com/tmspzz/Rome/tree/HEAD)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.24.0.65...HEAD)

**Merged pull requests:**

- Bump addressable from 2.7.0 to 2.8.0 [\#246](https://github.com/tmspzz/Rome/pull/246) ([dependabot[bot]](https://github.com/apps/dependabot))
- Bump rexml from 3.2.4 to 3.2.5 [\#245](https://github.com/tmspzz/Rome/pull/245) ([dependabot[bot]](https://github.com/apps/dependabot))
- Bump kramdown from 2.3.0 to 2.3.1 [\#243](https://github.com/tmspzz/Rome/pull/243) ([dependabot[bot]](https://github.com/apps/dependabot))

## [v0.24.0.65](https://github.com/tmspzz/Rome/tree/v0.24.0.65) (2021-11-15)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.23.3.64...v0.24.0.65)

**Closed issues:**

- Rome crashes when downloading dependencies [\#242](https://github.com/tmspzz/Rome/issues/242)
- When using currentMap its not possible to reuse the generated binaries [\#241](https://github.com/tmspzz/Rome/issues/241)
- Needs to be updated to support XCFrameworks. [\#238](https://github.com/tmspzz/Rome/issues/238)
- Issues running Rome with --no-skip-current [\#237](https://github.com/tmspzz/Rome/issues/237)
- Static folder isn't uploaded to remote server on Xcode 12 [\#233](https://github.com/tmspzz/Rome/issues/233)
- Losing debug capability when using cached builds downloaded by Rome [\#232](https://github.com/tmspzz/Rome/issues/232)
- Xcode version specific upload/download [\#227](https://github.com/tmspzz/Rome/issues/227)

**Merged pull requests:**

- Add Support for XCFrameworks [\#247](https://github.com/tmspzz/Rome/pull/247) ([vikrem](https://github.com/vikrem))
- Add more details to current map section of README [\#240](https://github.com/tmspzz/Rome/pull/240) ([mpdifran](https://github.com/mpdifran))
- Bump kramdown from 2.2.1 to 2.3.0 [\#231](https://github.com/tmspzz/Rome/pull/231) ([dependabot[bot]](https://github.com/apps/dependabot))
- Update to stackage LTS 15.11 [\#226](https://github.com/tmspzz/Rome/pull/226) ([tmspzz](https://github.com/tmspzz))

## [v0.23.3.64](https://github.com/tmspzz/Rome/tree/v0.23.3.64) (2020-04-29)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.23.2.63...v0.23.3.64)

**Implemented enhancements:**

- Add CHANGELOG.md [\#223](https://github.com/tmspzz/Rome/issues/223)
- AWS role based access is not working [\#210](https://github.com/tmspzz/Rome/issues/210)
- Reduce verbosity by hiding delete errors [\#219](https://github.com/tmspzz/Rome/pull/219) ([tmspzz](https://github.com/tmspzz))

**Fixed bugs:**

- Remove query parameters from repo name parsed from binary Cartfile entry [\#222](https://github.com/tmspzz/Rome/pull/222) ([ffittschen](https://github.com/ffittschen))

**Closed issues:**

- Framework always missing using custom-engine [\#225](https://github.com/tmspzz/Rome/issues/225)
- Rome confusing versions of parent & transient dependency [\#221](https://github.com/tmspzz/Rome/issues/221)
- Getting "the specified key does not exist" after changing AWS access key [\#217](https://github.com/tmspzz/Rome/issues/217)
- Using rome upload fails before calling engine [\#215](https://github.com/tmspzz/Rome/issues/215)

**Merged pull requests:**

- Add CHANGELOG.md \(\#223\) [\#224](https://github.com/tmspzz/Rome/pull/224) ([didix21](https://github.com/didix21))
- Fix link in README.md [\#220](https://github.com/tmspzz/Rome/pull/220) ([devxoul](https://github.com/devxoul))

## [v0.23.2.63](https://github.com/tmspzz/Rome/tree/v0.23.2.63) (2020-01-27)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.23.1.61...v0.23.2.63)

**Closed issues:**

- Repository map for multiple schemes [\#213](https://github.com/tmspzz/Rome/issues/213)
- Download from publicly readable S3 bucket with no credentials [\#208](https://github.com/tmspzz/Rome/issues/208)
- Caching targets that have no binary [\#205](https://github.com/tmspzz/Rome/issues/205)
- S3 Errors on Download: The specified key does not exist. [\#202](https://github.com/tmspzz/Rome/issues/202)
- rome download when using a custom engine doesnt place items in Carthage directory [\#195](https://github.com/tmspzz/Rome/issues/195)

**Merged pull requests:**

- Proper quoting for argument passed by xargs [\#212](https://github.com/tmspzz/Rome/pull/212) ([vytautasgimbutas](https://github.com/vytautasgimbutas))
- Read aws\_session\_token and aws\_expiration [\#211](https://github.com/tmspzz/Rome/pull/211) ([tmspzz](https://github.com/tmspzz))
- Updated google enginee zip\_file [\#207](https://github.com/tmspzz/Rome/pull/207) ([Macarse](https://github.com/Macarse))
- Instruct engine to download files to a temporary path [\#206](https://github.com/tmspzz/Rome/pull/206) ([tmspzz](https://github.com/tmspzz))
- Fix command typo in README [\#199](https://github.com/tmspzz/Rome/pull/199) ([a2](https://github.com/a2))
- add a google example engine [\#196](https://github.com/tmspzz/Rome/pull/196) ([ekimia](https://github.com/ekimia))

## [v0.23.1.61](https://github.com/tmspzz/Rome/tree/v0.23.1.61) (2019-06-19)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.23.0.60...v0.23.1.61)

**Breaking changes:**

- S3 - Moving role\_arn from `credentials` to `config` [\#189](https://github.com/tmspzz/Rome/issues/189)

**Closed issues:**

- The bucket must be using the specified endpoint [\#191](https://github.com/tmspzz/Rome/issues/191)
- "The AWS Access Key Id you provided does not exist in our records" on minio instances. [\#187](https://github.com/tmspzz/Rome/issues/187)

**Merged pull requests:**

- Read endpoint url from proper file [\#193](https://github.com/tmspzz/Rome/pull/193) ([tmspzz](https://github.com/tmspzz))
- Add cache prefix to remote framework upload path with engine [\#192](https://github.com/tmspzz/Rome/pull/192) ([BalestraPatrick](https://github.com/BalestraPatrick))

## [v0.23.0.60](https://github.com/tmspzz/Rome/tree/v0.23.0.60) (2019-06-05)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.22.0.59...v0.23.0.60)

**Implemented enhancements:**

- Support synchronizing with a custom script / executable [\#146](https://github.com/tmspzz/Rome/issues/146)
- Support Nexus repository [\#139](https://github.com/tmspzz/Rome/issues/139)
- Allow un-authententicated download access to S3 repository [\#126](https://github.com/tmspzz/Rome/issues/126)
- Support for Bintray [\#120](https://github.com/tmspzz/Rome/issues/120)
- Add support for rsync and/or scp [\#116](https://github.com/tmspzz/Rome/issues/116)
- Add Support for Google Cloud Storage [\#49](https://github.com/tmspzz/Rome/issues/49)

**Closed issues:**

- Cannot use Rome with Minio [\#186](https://github.com/tmspzz/Rome/issues/186)

**Merged pull requests:**

- S3 - Using Profile with role\_arn from config over credential file [\#190](https://github.com/tmspzz/Rome/pull/190) ([popei69](https://github.com/popei69))

## [v0.22.0.59](https://github.com/tmspzz/Rome/tree/v0.22.0.59) (2019-05-18)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.21.0.58...v0.22.0.59)

**Implemented enhancements:**

- S3 - Profile with role\_arn are not working [\#175](https://github.com/tmspzz/Rome/issues/175)

**Merged pull requests:**

- Add Custom Script Engine [\#185](https://github.com/tmspzz/Rome/pull/185) ([BalestraPatrick](https://github.com/BalestraPatrick))

## [v0.21.0.58](https://github.com/tmspzz/Rome/tree/v0.21.0.58) (2019-05-15)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.21.0.57...v0.21.0.58)

**Merged pull requests:**

- Fix a spelling typo in README.md under 'Set up' [\#183](https://github.com/tmspzz/Rome/pull/183) ([SteveOfTheStow](https://github.com/SteveOfTheStow))
- Add role\_arn support [\#179](https://github.com/tmspzz/Rome/pull/179) ([tmspzz](https://github.com/tmspzz))
- Fix warnings [\#176](https://github.com/tmspzz/Rome/pull/176) ([fabb](https://github.com/fabb))

## [v0.21.0.57](https://github.com/tmspzz/Rome/tree/v0.21.0.57) (2019-04-13)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.20.0.56...v0.21.0.57)

## [v0.20.0.56](https://github.com/tmspzz/Rome/tree/v0.20.0.56) (2019-03-11)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.19.0.55...v0.20.0.56)

**Implemented enhancements:**

- Parallel download/upload of binaries from S3. [\#158](https://github.com/tmspzz/Rome/issues/158)

**Fixed bugs:**

- Rome does not recognize last entry in Cartfile.resolved if there's no new line at end [\#165](https://github.com/tmspzz/Rome/issues/165)
- Rome crashes when downloading cache from remote - Zero bytes Archive [\#125](https://github.com/tmspzz/Rome/issues/125)

**Merged pull requests:**

- Parallelize downloads and uploads [\#174](https://github.com/tmspzz/Rome/pull/174) ([tmspzz](https://github.com/tmspzz))
- Hiding some log messages behind verbose [\#173](https://github.com/tmspzz/Rome/pull/173) ([tmspzz](https://github.com/tmspzz))
- Update to lts 13.10 [\#172](https://github.com/tmspzz/Rome/pull/172) ([tmspzz](https://github.com/tmspzz))
- Do not donwload current if not mentioned [\#171](https://github.com/tmspzz/Rome/pull/171) ([tmspzz](https://github.com/tmspzz))
- Don't unzip zero bytes archives [\#170](https://github.com/tmspzz/Rome/pull/170) ([tmspzz](https://github.com/tmspzz))
- Fix typos [\#169](https://github.com/tmspzz/Rome/pull/169) ([shingt](https://github.com/shingt))
- Fix parse function typo [\#168](https://github.com/tmspzz/Rome/pull/168) ([samritchie](https://github.com/samritchie))
- Fix README layout [\#167](https://github.com/tmspzz/Rome/pull/167) ([shingt](https://github.com/shingt))

## [v0.19.0.55](https://github.com/tmspzz/Rome/tree/v0.19.0.55) (2019-02-16)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.18.1.53...v0.19.0.55)

**Merged pull requests:**

- Handle Cartfile entries with no new lines at EOL [\#166](https://github.com/tmspzz/Rome/pull/166) ([tmspzz](https://github.com/tmspzz))

## [v0.18.1.53](https://github.com/tmspzz/Rome/tree/v0.18.1.53) (2019-02-07)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.18.1.52...v0.18.1.53)

**Fixed bugs:**

- currentMap ignored when using `rome upload \<current\_framework\_repo\_name\>` [\#160](https://github.com/tmspzz/Rome/issues/160)

**Merged pull requests:**

- Fix current named upload/download [\#161](https://github.com/tmspzz/Rome/pull/161) ([tmspzz](https://github.com/tmspzz))

## [v0.18.1.52](https://github.com/tmspzz/Rome/tree/v0.18.1.52) (2019-01-14)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.18.0.51...v0.18.1.52)

**Implemented enhancements:**

- Upload the current framework when available [\#24](https://github.com/tmspzz/Rome/issues/24)

## [v0.18.0.51](https://github.com/tmspzz/Rome/tree/v0.18.0.51) (2018-11-09)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.17.2.50...v0.18.0.51)

**Closed issues:**

- Typo in README [\#151](https://github.com/tmspzz/Rome/issues/151)

**Merged pull requests:**

- Add CurrentMap and --no-skip-current support [\#156](https://github.com/tmspzz/Rome/pull/156) ([tmspzz](https://github.com/tmspzz))
- Add AppUnite article and fix cache prefixing [\#153](https://github.com/tmspzz/Rome/pull/153) ([SzymonMrozek](https://github.com/SzymonMrozek))
- Fixed repositoryMap typos in the README [\#152](https://github.com/tmspzz/Rome/pull/152) ([chika-kasymov](https://github.com/chika-kasymov))

## [v0.17.2.50](https://github.com/tmspzz/Rome/tree/v0.17.2.50) (2018-09-09)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.17.1.49...v0.17.2.50)

**Implemented enhancements:**

- Support multiple configurations per project [\#143](https://github.com/tmspzz/Rome/issues/143)
- Rome incorrectly assumes supported platform [\#135](https://github.com/tmspzz/Rome/issues/135)

**Fixed bugs:**

- Rome do not use env variables [\#149](https://github.com/tmspzz/Rome/issues/149)

**Merged pull requests:**

- Fix/149 env variables [\#150](https://github.com/tmspzz/Rome/pull/150) ([tmspzz](https://github.com/tmspzz))
- Add --romefile CLI option to all commands [\#147](https://github.com/tmspzz/Rome/pull/147) ([tmspzz](https://github.com/tmspzz))

## [v0.17.1.49](https://github.com/tmspzz/Rome/tree/v0.17.1.49) (2018-08-24)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.17.0.48...v0.17.1.49)

## [v0.17.0.48](https://github.com/tmspzz/Rome/tree/v0.17.0.48) (2018-08-24)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.17.0.47...v0.17.0.48)

**Merged pull requests:**

- Add \[TargetPlatforms\] to Frameworks [\#141](https://github.com/tmspzz/Rome/pull/141) ([tmspzz](https://github.com/tmspzz))

## [v0.17.0.47](https://github.com/tmspzz/Rome/tree/v0.17.0.47) (2018-08-14)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.16.0.46...v0.17.0.47)

**Merged pull requests:**

- Add docs for Romefile YAML [\#140](https://github.com/tmspzz/Rome/pull/140) ([tmspzz](https://github.com/tmspzz))

## [v0.16.0.46](https://github.com/tmspzz/Rome/tree/v0.16.0.46) (2018-07-30)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.16.0.45...v0.16.0.46)

**Fixed bugs:**

- Not downloading existing bcsymbolmap files [\#131](https://github.com/tmspzz/Rome/issues/131)

**Merged pull requests:**

- Add carthage 0.30.1 static framework support [\#132](https://github.com/tmspzz/Rome/pull/132) ([tmspzz](https://github.com/tmspzz))

## [v0.16.0.45](https://github.com/tmspzz/Rome/tree/v0.16.0.45) (2018-07-09)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.16.0.44...v0.16.0.45)

**Merged pull requests:**

- Add Mozilla to Who uses Rome? [\#130](https://github.com/tmspzz/Rome/pull/130) ([tmspzz](https://github.com/tmspzz))
- Update to lts 11.13 [\#128](https://github.com/tmspzz/Rome/pull/128) ([tmspzz](https://github.com/tmspzz))

## [v0.16.0.44](https://github.com/tmspzz/Rome/tree/v0.16.0.44) (2018-06-19)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.15.0.43...v0.16.0.44)

**Implemented enhancements:**

- Support environment variables for AWS Region and AWS Endpoint override  [\#121](https://github.com/tmspzz/Rome/issues/121)
- Add support for S3 Bucket URL [\#118](https://github.com/tmspzz/Rome/issues/118)

**Closed issues:**

- Prefix by Swift Version by Default [\#127](https://github.com/tmspzz/Rome/issues/127)

**Merged pull requests:**

- Add more users to Who uses Rome [\#124](https://github.com/tmspzz/Rome/pull/124) ([tmspzz](https://github.com/tmspzz))
- Add cocoapods and other improvements [\#123](https://github.com/tmspzz/Rome/pull/123) ([tmspzz](https://github.com/tmspzz))
- Add Rome.podspec [\#122](https://github.com/tmspzz/Rome/pull/122) ([thii](https://github.com/thii))
- Add Minio support via AWS Service Endpoint override [\#119](https://github.com/tmspzz/Rome/pull/119) ([tmspzz](https://github.com/tmspzz))

## [v0.15.0.43](https://github.com/tmspzz/Rome/tree/v0.15.0.43) (2018-05-23)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.15.0.42...v0.15.0.43)

## [v0.15.0.42](https://github.com/tmspzz/Rome/tree/v0.15.0.42) (2018-05-22)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.15.0.41...v0.15.0.42)

## [v0.15.0.41](https://github.com/tmspzz/Rome/tree/v0.15.0.41) (2018-05-22)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.15.0.40...v0.15.0.41)

## [v0.15.0.40](https://github.com/tmspzz/Rome/tree/v0.15.0.40) (2018-05-03)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.14.1.39...v0.15.0.40)

**Fixed bugs:**

- Error when downloading framework to cache [\#114](https://github.com/tmspzz/Rome/issues/114)

**Merged pull requests:**

- Fix save path of bcsymbolmap in local cache [\#115](https://github.com/tmspzz/Rome/pull/115) ([tmspzz](https://github.com/tmspzz))

## [v0.14.1.39](https://github.com/tmspzz/Rome/tree/v0.14.1.39) (2018-01-23)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.14.0.38...v0.14.1.39)

**Fixed bugs:**

- Symlinks not preserved [\#112](https://github.com/tmspzz/Rome/issues/112)

**Merged pull requests:**

- Fix symlinks not preserved [\#113](https://github.com/tmspzz/Rome/pull/113) ([tmspzz](https://github.com/tmspzz))

## [v0.14.0.38](https://github.com/tmspzz/Rome/tree/v0.14.0.38) (2018-01-22)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.14.0.37...v0.14.0.38)

**Implemented enhancements:**

- IgnoreMap should be ignored when listing rome missing dependencies [\#107](https://github.com/tmspzz/Rome/issues/107)

## [v0.14.0.37](https://github.com/tmspzz/Rome/tree/v0.14.0.37) (2018-01-17)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.14.0.36...v0.14.0.37)

## [v0.14.0.36](https://github.com/tmspzz/Rome/tree/v0.14.0.36) (2018-01-12)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.13.1.35...v0.14.0.36)

**Implemented enhancements:**

- Include binary-only dependencies in Rome cache [\#110](https://github.com/tmspzz/Rome/issues/110)

**Closed issues:**

- Frameworks with symlinks inside are not uploaded correctly \[zlib\] \[common-crypto\] [\#106](https://github.com/tmspzz/Rome/issues/106)

**Merged pull requests:**

- Cache binary dependencies [\#111](https://github.com/tmspzz/Rome/pull/111) ([tmspzz](https://github.com/tmspzz))
- Add --no-ignore flag [\#108](https://github.com/tmspzz/Rome/pull/108) ([tmspzz](https://github.com/tmspzz))

## [v0.13.1.35](https://github.com/tmspzz/Rome/tree/v0.13.1.35) (2018-01-04)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.13.1.34...v0.13.1.35)

## [v0.13.1.34](https://github.com/tmspzz/Rome/tree/v0.13.1.34) (2017-12-19)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.13.0.33...v0.13.1.34)

**Implemented enhancements:**

- Cache bcsymbolmap files [\#100](https://github.com/tmspzz/Rome/issues/100)

**Merged pull requests:**

- Create CODE\_OF\_CONDUCT.md [\#105](https://github.com/tmspzz/Rome/pull/105) ([tmspzz](https://github.com/tmspzz))
- Add test for dwarf UUID parsing [\#104](https://github.com/tmspzz/Rome/pull/104) ([futtetennista](https://github.com/futtetennista))

## [v0.13.0.33](https://github.com/tmspzz/Rome/tree/v0.13.0.33) (2017-10-07)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.13.0.32...v0.13.0.33)

**Implemented enhancements:**

- Add JSON output format to list command [\#78](https://github.com/tmspzz/Rome/issues/78)

**Merged pull requests:**

- Add JSON output format to list command [\#103](https://github.com/tmspzz/Rome/pull/103) ([pt2121](https://github.com/pt2121))
- Support upload/download of bcsymbolmaps [\#101](https://github.com/tmspzz/Rome/pull/101) ([tmspzz](https://github.com/tmspzz))

## [v0.13.0.32](https://github.com/tmspzz/Rome/tree/v0.13.0.32) (2017-09-04)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.12.0.31...v0.13.0.32)

**Implemented enhancements:**

- Parallelise list command network calls [\#79](https://github.com/tmspzz/Rome/issues/79)

**Closed issues:**

- Different build paths for Xcode 8 and 9 [\#82](https://github.com/tmspzz/Rome/issues/82)

**Merged pull requests:**

- Make Rome build with lts-9.0 \(\#98\) [\#99](https://github.com/tmspzz/Rome/pull/99) ([tmspzz](https://github.com/tmspzz))
- Parallelise s3 cache probing [\#97](https://github.com/tmspzz/Rome/pull/97) ([mheinzel](https://github.com/mheinzel))
- Refactoring [\#96](https://github.com/tmspzz/Rome/pull/96) ([mheinzel](https://github.com/mheinzel))
- Fix typo in README [\#93](https://github.com/tmspzz/Rome/pull/93) ([msanders](https://github.com/msanders))
- Add vendor/bundle to cached directories in Travis [\#90](https://github.com/tmspzz/Rome/pull/90) ([tmspzz](https://github.com/tmspzz))

## [v0.12.0.31](https://github.com/tmspzz/Rome/tree/v0.12.0.31) (2017-07-11)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.12.0.30...v0.12.0.31)

**Merged pull requests:**

- Update version badge [\#89](https://github.com/tmspzz/Rome/pull/89) ([tmspzz](https://github.com/tmspzz))
- v0.12.0.31 - Spurius Lartius [\#84](https://github.com/tmspzz/Rome/pull/84) ([tmspzz](https://github.com/tmspzz))

## [v0.12.0.30](https://github.com/tmspzz/Rome/tree/v0.12.0.30) (2017-07-11)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.12.0.29...v0.12.0.30)

**Merged pull requests:**

- Save to local cache using prefix [\#85](https://github.com/tmspzz/Rome/pull/85) ([tmspzz](https://github.com/tmspzz))

## [v0.12.0.29](https://github.com/tmspzz/Rome/tree/v0.12.0.29) (2017-07-10)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.11.0.27...v0.12.0.29)

**Implemented enhancements:**

- Can I use local cache only? [\#61](https://github.com/tmspzz/Rome/issues/61)

**Closed issues:**

- Support for carthage packages that have multiple framework output [\#80](https://github.com/tmspzz/Rome/issues/80)
- Framework upload to Amazon's S3 failure [\#73](https://github.com/tmspzz/Rome/issues/73)

**Merged pull requests:**

- Introduce CachePrefix [\#83](https://github.com/tmspzz/Rome/pull/83) ([tmspzz](https://github.com/tmspzz))
- Add Danger.Systems to run HLint [\#81](https://github.com/tmspzz/Rome/pull/81) ([tmspzz](https://github.com/tmspzz))
- Get rid of compiler warnings [\#77](https://github.com/tmspzz/Rome/pull/77) ([mheinzel](https://github.com/mheinzel))
- Add latest version check when running download or upload [\#75](https://github.com/tmspzz/Rome/pull/75) ([tmspzz](https://github.com/tmspzz))
- Update README.md [\#74](https://github.com/tmspzz/Rome/pull/74) ([tmspzz](https://github.com/tmspzz))
- Avoid multiple redeclarations of sayFunc [\#72](https://github.com/tmspzz/Rome/pull/72) ([tmspzz](https://github.com/tmspzz))
- Add CI workflow [\#71](https://github.com/tmspzz/Rome/pull/71) ([tmspzz](https://github.com/tmspzz))
- Add fastlane badge [\#68](https://github.com/tmspzz/Rome/pull/68) ([tmspzz](https://github.com/tmspzz))
- Fix/readme update local steps [\#67](https://github.com/tmspzz/Rome/pull/67) ([tmspzz](https://github.com/tmspzz))
- Add total downloads badge [\#66](https://github.com/tmspzz/Rome/pull/66) ([tmspzz](https://github.com/tmspzz))
- Add release badge to README.md [\#65](https://github.com/tmspzz/Rome/pull/65) ([tmspzz](https://github.com/tmspzz))

## [v0.11.0.27](https://github.com/tmspzz/Rome/tree/v0.11.0.27) (2017-05-10)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.11.0.26...v0.11.0.27)

**Fixed bugs:**

- Rome places files within frameworks individually rather than replacing entire framework on download [\#64](https://github.com/tmspzz/Rome/issues/64)

**Merged pull requests:**

- Allow stand alone local cache [\#63](https://github.com/tmspzz/Rome/pull/63) ([tmspzz](https://github.com/tmspzz))
- Add troubleshooting section [\#62](https://github.com/tmspzz/Rome/pull/62) ([Palleas](https://github.com/Palleas))

## [v0.11.0.26](https://github.com/tmspzz/Rome/tree/v0.11.0.26) (2017-05-08)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.11.0.25...v0.11.0.26)

## [v0.11.0.25](https://github.com/tmspzz/Rome/tree/v0.11.0.25) (2017-04-26)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.10.2.24...v0.11.0.25)

**Implemented enhancements:**

- Rome fails with binary references in Cartfile.resolved, still exits cleanly [\#58](https://github.com/tmspzz/Rome/issues/58)

**Fixed bugs:**

- Missing executable permission to framework [\#57](https://github.com/tmspzz/Rome/issues/57)

**Merged pull requests:**

- Skipping malformed lines in Cartfile.resolved [\#59](https://github.com/tmspzz/Rome/pull/59) ([tmspzz](https://github.com/tmspzz))
- Fix typo in doc [\#55](https://github.com/tmspzz/Rome/pull/55) ([Palleas](https://github.com/Palleas))

## [v0.10.2.24](https://github.com/tmspzz/Rome/tree/v0.10.2.24) (2017-04-07)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.10.2.23...v0.10.2.24)

**Merged pull requests:**

- Chmod frameworks [\#60](https://github.com/tmspzz/Rome/pull/60) ([tmspzz](https://github.com/tmspzz))

## [v0.10.2.23](https://github.com/tmspzz/Rome/tree/v0.10.2.23) (2017-04-04)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.10.1.22...v0.10.2.23)

**Merged pull requests:**

- Fix/carthage-mac-dir [\#54](https://github.com/tmspzz/Rome/pull/54) ([tmspzz](https://github.com/tmspzz))

## [v0.10.1.22](https://github.com/tmspzz/Rome/tree/v0.10.1.22) (2017-03-30)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.10.0.21...v0.10.1.22)

**Implemented enhancements:**

- Support for carthage --cache-builds flag [\#47](https://github.com/tmspzz/Rome/issues/47)

**Merged pull requests:**

- Add syntax highlighting to Readme [\#52](https://github.com/tmspzz/Rome/pull/52) ([BasThomas](https://github.com/BasThomas))
- Adds support for Version File caching [\#48](https://github.com/tmspzz/Rome/pull/48) ([tmspzz](https://github.com/tmspzz))

## [v0.10.0.21](https://github.com/tmspzz/Rome/tree/v0.10.0.21) (2017-03-19)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.9.0.20...v0.10.0.21)

**Merged pull requests:**

- Adding tons of documentation removing unsued code [\#46](https://github.com/tmspzz/Rome/pull/46) ([tmspzz](https://github.com/tmspzz))
- Revisiting Project Structure [\#45](https://github.com/tmspzz/Rome/pull/45) ([tmspzz](https://github.com/tmspzz))

## [v0.9.0.20](https://github.com/tmspzz/Rome/tree/v0.9.0.20) (2017-02-12)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.9.0.19...v0.9.0.20)

**Implemented enhancements:**

- Add support for multiple platforms [\#36](https://github.com/tmspzz/Rome/issues/36)

**Merged pull requests:**

- Improving download verbosity [\#44](https://github.com/tmspzz/Rome/pull/44) ([tmspzz](https://github.com/tmspzz))

## [v0.9.0.19](https://github.com/tmspzz/Rome/tree/v0.9.0.19) (2017-02-07)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.9.0.18...v0.9.0.19)

**Merged pull requests:**

- Use git repo name instead of framework name in cache directory layout [\#43](https://github.com/tmspzz/Rome/pull/43) ([r-peck](https://github.com/r-peck))

## [v0.9.0.18](https://github.com/tmspzz/Rome/tree/v0.9.0.18) (2017-02-06)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.8.0.17...v0.9.0.18)

**Implemented enhancements:**

- Feature Local Cache  [\#38](https://github.com/tmspzz/Rome/issues/38)

**Closed issues:**

- Deliver Rome via Homebrew [\#9](https://github.com/tmspzz/Rome/issues/9)

**Merged pull requests:**

- Feature/multiple platforms [\#42](https://github.com/tmspzz/Rome/pull/42) ([r-peck](https://github.com/r-peck))
- Feature/local cache [\#41](https://github.com/tmspzz/Rome/pull/41) ([tmspzz](https://github.com/tmspzz))

## [v0.8.0.17](https://github.com/tmspzz/Rome/tree/v0.8.0.17) (2016-11-30)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.8.0.16...v0.8.0.17)

## [v0.8.0.16](https://github.com/tmspzz/Rome/tree/v0.8.0.16) (2016-11-19)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.8.0.15...v0.8.0.16)

## [v0.8.0.15](https://github.com/tmspzz/Rome/tree/v0.8.0.15) (2016-11-16)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.7.1.14...v0.8.0.15)

**Fixed bugs:**

- Entries in Cartfile.resolved after empty lines are ignored [\#40](https://github.com/tmspzz/Rome/issues/40)

**Merged pull requests:**

- Ignoring empty lines in Cartfile.resolved [\#39](https://github.com/tmspzz/Rome/pull/39) ([tmspzz](https://github.com/tmspzz))

## [v0.7.1.14](https://github.com/tmspzz/Rome/tree/v0.7.1.14) (2016-10-18)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.7.1.13...v0.7.1.14)

**Implemented enhancements:**

- Add support for multiple platforms [\#35](https://github.com/tmspzz/Rome/issues/35)
- Allow for certain repositories to be ignored entirely [\#31](https://github.com/tmspzz/Rome/issues/31)

**Closed issues:**

- Add support for uploading resulting build [\#37](https://github.com/tmspzz/Rome/issues/37)

**Merged pull requests:**

- Feature/ignore [\#34](https://github.com/tmspzz/Rome/pull/34) ([tmspzz](https://github.com/tmspzz))

## [v0.7.1.13](https://github.com/tmspzz/Rome/tree/v0.7.1.13) (2016-10-09)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.7.0.12...v0.7.1.13)

**Closed issues:**

- Rome upload does not accept repository name \(if differs from framework name\) [\#30](https://github.com/tmspzz/Rome/issues/30)

**Merged pull requests:**

- Add tests for filterByName and splitWithSeparator [\#33](https://github.com/tmspzz/Rome/pull/33) ([pt2121](https://github.com/pt2121))
- Feature/repo names instead of framework names [\#32](https://github.com/tmspzz/Rome/pull/32) ([tmspzz](https://github.com/tmspzz))

## [v0.7.0.12](https://github.com/tmspzz/Rome/tree/v0.7.0.12) (2016-10-08)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.6.1.11...v0.7.0.12)

**Closed issues:**

- Refactor Stringly Typed Functions to Properly typed functions [\#25](https://github.com/tmspzz/Rome/issues/25)
- Set up Travis for Rome [\#10](https://github.com/tmspzz/Rome/issues/10)

**Merged pull requests:**

- add travis config [\#29](https://github.com/tmspzz/Rome/pull/29) ([MichaelXavier](https://github.com/MichaelXavier))
- Adding issue template [\#28](https://github.com/tmspzz/Rome/pull/28) ([tmspzz](https://github.com/tmspzz))
- Fixes \#25 [\#26](https://github.com/tmspzz/Rome/pull/26) ([2016rshah](https://github.com/2016rshah))
- Added CONTRIBUTING.md [\#23](https://github.com/tmspzz/Rome/pull/23) ([tmspzz](https://github.com/tmspzz))
- Feature/improve verbosity [\#22](https://github.com/tmspzz/Rome/pull/22) ([tmspzz](https://github.com/tmspzz))
- Fix/readme [\#20](https://github.com/tmspzz/Rome/pull/20) ([tmspzz](https://github.com/tmspzz))

## [v0.6.1.11](https://github.com/tmspzz/Rome/tree/v0.6.1.11) (2016-09-28)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.6.0.10...v0.6.1.11)

**Closed issues:**

- One to many repository maps [\#12](https://github.com/tmspzz/Rome/issues/12)

**Merged pull requests:**

- Updating Readme [\#19](https://github.com/tmspzz/Rome/pull/19) ([tmspzz](https://github.com/tmspzz))
- Fix typo Donwloaded -\> Downloaded [\#18](https://github.com/tmspzz/Rome/pull/18) ([netbe](https://github.com/netbe))
- One-to-may framework names [\#17](https://github.com/tmspzz/Rome/pull/17) ([tmspzz](https://github.com/tmspzz))

## [v0.6.0.10](https://github.com/tmspzz/Rome/tree/v0.6.0.10) (2016-09-25)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.6.0.9...v0.6.0.10)

## [v0.6.0.9](https://github.com/tmspzz/Rome/tree/v0.6.0.9) (2016-09-09)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.6.0.8...v0.6.0.9)

## [v0.6.0.8](https://github.com/tmspzz/Rome/tree/v0.6.0.8) (2016-09-06)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.5.0.7...v0.6.0.8)

**Closed issues:**

- Symbols are not uploaded [\#11](https://github.com/tmspzz/Rome/issues/11)

## [v0.5.0.7](https://github.com/tmspzz/Rome/tree/v0.5.0.7) (2016-08-25)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.5.0.6...v0.5.0.7)

**Implemented enhancements:**

- Adopt proper INI format for Romefile [\#5](https://github.com/tmspzz/Rome/issues/5)

**Merged pull requests:**

- Feautre/matchin dsym [\#16](https://github.com/tmspzz/Rome/pull/16) ([tmspzz](https://github.com/tmspzz))
- Feature/romefile ini [\#14](https://github.com/tmspzz/Rome/pull/14) ([tmspzz](https://github.com/tmspzz))
- Fixed typo [\#13](https://github.com/tmspzz/Rome/pull/13) ([eugecm](https://github.com/eugecm))

## [v0.5.0.6](https://github.com/tmspzz/Rome/tree/v0.5.0.6) (2016-08-22)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.4.0.5...v0.5.0.6)

## [v0.4.0.5](https://github.com/tmspzz/Rome/tree/v0.4.0.5) (2016-08-21)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.4.0.4...v0.4.0.5)

**Implemented enhancements:**

- Romefile RepositoryMap does not work with github references [\#7](https://github.com/tmspzz/Rome/issues/7)

**Merged pull requests:**

- Have GitHub repos also use the RepositoryMap [\#8](https://github.com/tmspzz/Rome/pull/8) ([tmspzz](https://github.com/tmspzz))

## [v0.4.0.4](https://github.com/tmspzz/Rome/tree/v0.4.0.4) (2016-08-17)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.3.0.3...v0.4.0.4)

## [v0.3.0.3](https://github.com/tmspzz/Rome/tree/v0.3.0.3) (2016-08-17)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.3.0.2...v0.3.0.3)

**Implemented enhancements:**

- Unable to specify AWS s3 region [\#4](https://github.com/tmspzz/Rome/issues/4)
- Generating a list of non-uploaded dependencies to build to pass to Carthage [\#2](https://github.com/tmspzz/Rome/issues/2)

**Merged pull requests:**

- Feature/list [\#3](https://github.com/tmspzz/Rome/pull/3) ([tmspzz](https://github.com/tmspzz))

## [v0.3.0.2](https://github.com/tmspzz/Rome/tree/v0.3.0.2) (2016-08-16)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.3.0.1...v0.3.0.2)

**Merged pull requests:**

- Fix/region discrovery [\#6](https://github.com/tmspzz/Rome/pull/6) ([tmspzz](https://github.com/tmspzz))

## [v0.3.0.1](https://github.com/tmspzz/Rome/tree/v0.3.0.1) (2016-08-15)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.2.0.1...v0.3.0.1)

## [v0.2.0.1](https://github.com/tmspzz/Rome/tree/v0.2.0.1) (2016-08-14)

[Full Changelog](https://github.com/tmspzz/Rome/compare/v0.1.0.0...v0.2.0.1)

**Merged pull requests:**

- Fix typo [\#1](https://github.com/tmspzz/Rome/pull/1) ([marcoconti83](https://github.com/marcoconti83))

## [v0.1.0.0](https://github.com/tmspzz/Rome/tree/v0.1.0.0) (2016-08-08)

[Full Changelog](https://github.com/tmspzz/Rome/compare/6a602907283f93426e64749443ac6e101c4c614c...v0.1.0.0)



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
