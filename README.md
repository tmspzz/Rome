![](logo/colosseum.jpg)

# Rome [![Build Status](https://travis-ci.org/blender/Rome.svg?branch=master)](https://travis-ci.org/blender/Rome) [![rome-latest](https://img.shields.io/badge/release-v0.18.0.51-blue.svg)](https://github.com/blender/Rome/releases/tag/v0.18.0.51) ![cocoapods](https://img.shields.io/cocoapods/v/Rome.svg) ![total-downloads](https://img.shields.io/github/downloads/blender/Rome/total.svg) [![fastlane-plugin -badge](https://rawcdn.githack.com/fastlane/fastlane/master/fastlane/assets/plugin-badge.svg)](https://github.com/OpenShelter/fastlane-plugin-rome) [![twitter-follow](https://img.shields.io/twitter/follow/tmpz.svg?style=social&label=Follow)](https://twitter.com/tmpz)

Rome is a tool that allows developers on Apple platforms to use:

- Amazon's S3
- [Minio](https://www.minio.io/)
- [Ceph](https://ceph.com/ceph-storage/object-storage/)
- other S3 compatible object stores
- or/and a local folder

as a shared cache for frameworks built with [Carthage](https://github.com/Carthage/Carthage).

Trusted by: 

<a href="https://www.sharecare.com"><img src="https://www.hmnads.com/wp-content/uploads/2015/06/Sharecare-logo.png" alt="sharecare" height="90px"/></a> 
<a href="https://line.me"><img src="https://vignette.wikia.nocookie.net/starwars/images/b/b1/LINE_Corp_logo.png/revision/latest?cb=20170923181031" alt="linecorp" height="90px"/></a> 
<a href="https://www.daimler-tss.com"><img src="https://www.hdm-stuttgart.de/unternehmen/karrieremarktplatz/anmeldung/aussteller_uploads/99/dLlVePMVtu191516349018.png" alt="DaimlerTSS" height="90px"/></a> <a href="https://www.mozilla.com"><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Mozilla_logo.svg/2000px-Mozilla_logo.svg.png" alt="Mozilla" height="90px"/></a> <a href="https://www.brave.com"><img src="https://brave.com/fonts/brave-logotype-dark.svg" alt="Brave" height="90px"/></a>

[Search Github](https://github.com/search?utf8=%E2%9C%93&q=filename%3ARomefile&type=Code)

**Table of Contents**

- [Get Rome](#get-rome)
- [Use Rome with fastlane](#use-rome-with-fastlane)
- [The problem](#the-problem)
- [The solution](#the-solution)
- [Workflow](#workflow)
	- [Producer workflow](#producer-workflow)
	- [Consumer workflow](#consumer-workflow)
	- [CI workflow](#ci-workflow)
- [Set up](#set-up)
	- [Setting up AWS credentials](#setting-up-aws-credentials)
	- [Selecting the AWS Region](#selecting-the-aws-region)
	- [Setting up endpoint override for Minio, Ceph, or other S3 compatible stores](#setting-up-endpoint-override)
	- [Romefile](#romefile)
		- [Cache](#cache)
		- [RepositoryMap](#repositorymap)
		- [IgnoreMap](#ignoremap)
		- [CurrentMap](#currentmap)
		- [Multiple Aliases](#multiple-aliases)
		- [Static Frameworks](#static-frameworks)
		- [Platforms](#platforms)
	- [Cache Structure](#cache-structure)
		- [Cache Prefix](#cache-prefix)
- [Usage](#usage)
	- [Uploading](#uploading)
	- [Downloading](#downloading)
	- [Listing](#listing)
  - [Utils](#utils)
- [Troubleshooting & FAQ](#troubleshooting--faq)
	- [Getting "Image not found" when running an application using binaries](#getting-image-not-found-when-running-an-application-using-binaries)
	- [Supporting multiple Swift Versions](#supporting-multiple-swift-versions)
- [Developing](#developing)
- [Releasing](#releasing)
- [Presentations and Tutorials](#presentations-and-tutorials)
- [License](#license)

## Get Rome
### Using Homebrew
`$ brew install blender/homebrew-tap/rome`

### Using CocoaPods
Simply add the following line to your Podfile:
```
pod 'Rome'
```

This will download Rome to the `Pods/` folder during your next `pod install`
execution and will allow you to invoke it via `${PODS_ROOT}/Rome/rome` in your
Script Build Phases.

### Manual

The Rome binary is also attached as a zip to each release on the [releases page](https://github.com/blender/Rome/releases) here on GitHub.

Using Rome? Let me know by [opening an issue](https://github.com/blender/Rome/issues/new)
and I will gladly add you to the user list.

## Use Rome with fastlane

You can integrate Rome into your [fastlane](https://github.com/fastlane/fastlane) automation with the
[fastlane plugin for Rome](https://github.com/netbe/fastlane-plugin-rome).

## The problem

Suppose you're working a number of frameworks for your project and want to
share those with your team. A great way to do so is to use Carthage and
have team members point the `Cartfile` to the new framework version (or branch, tag, commit)
and run `carthage update`.

Unfortunately this will require them to build from scratch the new framework.
This is particularly annoying if the dependency tree for that framework is big
and / or takes a long time to build.

## The solution

Use a cache. The first team member (or a CI) can build the framework and share it, while all
other developers can get it from the cache with no waiting time.

## Workflow

The Rome's workflow changes depending if you are the producer (i.e. the first
person in your team to build the framework) or the consumer.

### Producer workflow

```bash
$ vi Cartfile # point to the new version of the framework
$ carthage update && rome upload
```

If you are running Rome in the context of a framework and want to [upload](#uploading) the current framework see [CurrentMap](#currentmap).

### Consumer workflow

```bash
$ vi Cartfile # point to the new version of the framework if necessary
$ carthage update --no-build && rome download
```

or

```bash
$ vi Cartfile.resolved # point to the new version of the framework
$ rome download
```

If you are running Rome in the context of a framework and want to [download](#downloading) the current framework see [CurrentMap](#currentmap).

### CI workflow

A CI can be both consumer and producer.

A simple workflow for using Rome on a continuous integration should resemble the
following:

- get available artifacts
- check if any artifacts are missing
- build missing artifacts if any
- upload build artifacts to the cache if needed

Or in code:

```
rome download --platform iOS # download missing frameworks (or copy from local cache)
rome list --missing --platform ios | awk '{print $1}' | xargs carthage update --platform ios --cache-builds # list what is missing and update/build if needed
rome list --missing --platform ios | awk '{print $1}' | xargs rome upload --platform ios # upload what is missing
```

If no frameworks are missing, the `awk` pipe to `carthage` will fail and the rest of the command will not be executed. This avoids rebuilding all dependencies or uploading artifacts already present in the cache.

You can use the [fastlane plugin for Rome](#use-rome-with-fastlane) to implement
a CI workflow too.

If you are running Rome in the context of a framework and want to [upload](#uploading) or [download](#downloading) the current framework see [CurrentMap](#currentmap).

## Set up

If you plan to use Amazon's S3 as a cache, then follow the next three steps:

- First you need a `.aws/credentials` file in your home folder. This is used to specify
your [AWS Credentials](#setting-up-aws-credentials).
- Second you need a `.aws/config` file in your home folder. This is used to specify the [AWS
Region](#selecting-the-aws-region).
- Third you need a [Romefile](#romefile) in the project where you want to use Rome. At the
same level where the `Cartfile` is.

If you just want to use only a local folder as a cache then:

- You need a [Romefile](#romefile) in the project where you want to use Rome. At the
same level where the `Cartfile` is. Since `0.17.1.49`, if you want to place the Romefile elsewhere or name it dirrentely
use `--romefile <path-to-romefile>` when running `rome <COMMAND>`.

### Setting up AWS credentials

Since version `0.2.0.0` Rome will expect to find credentials either as environment
variables `$AWS_ACCESS_KEY_ID` and `$AWS_SECRET_ACCESS_KEY` or in a file at
`.aws/credentials`. This aligns Rome behavior to other tools that use Amazon's SDK. See
[Amazon's blogpost on the topic](https://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs).

In your home folder create a `.aws/credentials` like the following

```
[default]
aws_access_key_id = ACCESS_KEY
aws_secret_access_key = SECRET_KEY
```

this should look something like

```
[default]
aws_access_key_id = AGIAJQARMD67CE3DTKHA
aws_secret_access_key = TedRV2/dFkBr1H3D7xuPsF9+CBHTjK0NKrJuoVs8
```

these will be the credentials that Rome will use to access S3 on your behalf.
To use configurations other than the `default` profile set the `$AWS_PROFILE`
environment variable to your desired profile.

### Selecting the AWS Region

In your home folder create a `.aws/config` like the following

```
[default]
region = us-east-1
```

To use configurations other than the `default` profile set the `$AWS_PROFILE`
environment variable to your desired profile.

Alternatively the AWS Region can also be specified by setting an `AWS_REGION` environment variable.

### Setting up endpoint override

To your `.aws/config` in the profile section you wish to use, add an `endpoint` key like so

```
[default]
region = us-east-1
endpoint = https://my.minio.host:9091
```

__Do not remove the `region` key__.

Default port for `https` endpoints is __443__ if the port is left unspecified.

Default port for `http` endpoints is __9000__ if the port is left unspecified.

Alternatively the endpoint can also be specified by setting an `AWS_ENDPOINT` environment variable.

### Romefile

#### About the format
Since version `0.17.0.48` the Romefile is in [YAML](https://learnxinyminutes.com/docs/yaml/) format.
Rome can still read the [INI](https://en.wikipedia.org/wiki/INI_file) Romefile, for now.

__Sucessive release might abandon compatibility__.

Feature support that require additions or changes to the Romefile __won't be supported in INI__.

You can migrate your Romefile to YAML by running `rome utils migrate-romefile`.

If you are looking for the documention prior to `0.17.0.48`, check the [wiki](https://github.com/blender/Rome/wiki/Romefile-prior-0.17.x.x)

#### Purpose

The Romefile has three purposes:

1. Specifies what caches to use - `cache` key. This key is __required__.
1. Allows to use custom name mappings between repository names and framework names - `repositoryMap` key. This key is __optional__ and can be omitted.
1. Allows to ignore certain framework names - `ignoreMap` key. This key is __optional__ and can be omitted.
1. Allows to specify the current framework's name(s) - `currentMap` key. This key is __optional__ and can be omitted.

#### Structure

A Romefile is made of 4 objects, of which only one, the `cache`, is required.

- A `cache` definition object
- A `repositoryMap` made of a list of `Romefile Entry`
- An `ignoreMap` made of a list of `Romefile Entry`
- A ``currentMap` made of a list of `Romefile Entry`

Each `Romefile Entry` is made of:

- A `name`
- A `type` which can be `static` or `dynamic`
- A set of supported `platforms` including `iOS`, `Mac`, `tvOS`, `watchOS`

A Romefile looks like this:

```yaml
cache: # required
  local: ~/Library/Caches/Rome # optional
                               # at least one between `local` and `s3Bucket` is required
  s3Bucket: ios-dev-bucket # optional
                           # at least one between `local` and `s3Bucket` is required 
repositoryMap: # optional
- better-dog-names: # entry that does not follow
                    # the "Organization/FrameworkName" convention.
  - name: DogFramework # required
    type: static # optional, defaults to dynamic
- HockeySDK-iOS:
  - name: HockeySDK
    platforms: [iOS] # optional, all platforms if empty
- awesome-framework-for-cat-names:
  - name: CatFramework
    type: dynamic
ignoreMap:
- GDCWebServer:
  - name: GDCWebServer
currentMap:
- animal-names-framework:
  - name: AnimalNames
```

#### Cache
The cache __must__ contain __at least one__ between:
- the name of the S3 Bucket to upload/download to/from. The key `s3Bucket` is __optional__.
- the path to local directory to use as an additional cache. The key `local` is __optional__.

```yaml
cache: # required
  local: ~/Library/Caches/Rome # optional
                               # at least one between `local` and `s3Bucket` is required
  s3Bucket: ios-dev-bucket # optional
                           # at least one between `local` and `s3Bucket` is required 
```

This is already a viable Romefile.

#### RepositoryMap
This contains the mappings of repository and framework names.
This is particularly useful if dependecies are not on GitHub or don't respect the "Organization/FrameworkName" convention.

Example:

Suppose you have the following in your `Cartfile`

```
github "Alamofire/Alamofire" ~> 4.3.0
github "bitstadium/HockeySDK-iOS" "3.8.6"
git "http://stash.myAnimalStartup.com/scm/iossdk/awesome-framework-for-cat-names.git" ~> 3.3.1
git "http://stash.myAnimalStartup.com/scm/iossdk/better-dog-names.git" ~> 0.4.4
```

which translates to the following `Cartfile.resolved`

```
github "Alamofire/Alamofire" "4.3.0"
github "bitstadium/HockeySDK-iOS" "3.8.6"
git "http://stash.myAnimalStartup.com/scm/iossdk/awesome-framework-for-cat-names.git" "3.3.1"
git "http://stash.myAnimalStartup.com/scm/iossdk/better-dog-names.git" "0.4.4"
```

but your framework names are actually `HockeySDK`, `CatFramework` and `DogFramework`
as opposed to `HockeySDK-iOS`, `awesome-framework-for-cat-names` and `better-dog-names`.

simply add a `repositoryMap` key to your `Romefile` and specify the following mapping:

```yaml
cache:
  local: ~/Library/Caches/Rome 
repositoryMap:
- better-dog-names: # this is the Romefile Entry for  `better-dog-names`
  - name: DogFramework
    type: static
    platforms: [iOS, Mac]
- HockeySDK-iOS: # this is the Romefile Entry for  `HockeySDK-iOS`
  - name: HockeySDK
    platforms: [iOS]
- awesome-framework-for-cat-names:  # this is the Romefile Entry for  `awesome-framework-for-cat-names`
  - name: CatFramework
  - type: dynamic
```

Note that __it was not necessary to add Alamofire__ as it respects the "Organization/FrameworkName" convention.

#### IgnoreMap
This contains the mappings of repository and framework names that should be ignored.
This is particularly useful in case not all your `Cartfile.resolved` entries produce a framework.

Some repositories use Carthage as a simple mechanism to include other git repositories that do not produce frameworks.
Even Carthage itself does this, to include xcconfigs.


Example:

Suppose you have the following in your `Cartfile`

```
github "Quick/Nimble"
github "jspahrsummers/xcconfigs"
```

`xcconfigs` can be ignored by Rome by adding an `ignoreMap` key in the Romefile


```yaml
ignoreMap:
- xcconfigs:
  - name: xcconfigs
```

Each entry in the `IgnoreMap` is also a `Romefile Entry` and supports all keys.

#### CurrentMap
__By default the `currentMap` is not used__. Specify `--no-skip-current` as a command line option to use it.
It is supported by Rome versions greater than `0.18.x.y` and can be specified only in YAML. 

The `currentMap` contains the mappings of repository and framework name(s) that describe the current framework.
This is particularly useful if you want to use Rome in the context of a framework. It is the [equivalent](https://github.com/Carthage/Carthage#share-your-xcode-schemes) of
[Carthage's `--no-skip-current`](https://github.com/Carthage/Carthage#use-travis-ci-to-upload-your-tagged-prebuilt-frameworks).

```yaml
currentMap:
- Alamofire:
  - name: Alamofire
```

Each entry in the `currentMap` is also a `Romefile Entry` and supports all keys.

The `currentMap` is __subject to the ignores specified in the `ignoreMap`__. 
If you explicitly specify names of frameworks to [upload](#uploading), [download](#downloading) on the command line, 
you don't need to pass `--no-skip-current` to use the `currentMap`. Just specify the name(s) of the current framework.

The version of the current framework is determined by

```
git describe --tags --exact-match `git rev-parse HEAD`
```

__If the commands does not resolve to any tag, the HEAD commit hash from `git rev-parse HEAD` is used as version.__

#### Multiple Aliases

Suppose you have a framework `Framework` that builds two targets, `t1` and `t2`,
Rome can handle both targets by specifying

```yaml
repositoryMap:
- Framework:
  - name: t1
  - name: t2
```

__Note__: if __ANY__ of the aliases is missing on S3, the entire entry will be reported as missing
when running `rome list [--missing]`

Multiple aliases are supported in `ignoreMap` too

#### Static Frameworks

Since version [0.30.1](https://github.com/Carthage/Carthage/releases/tag/0.30.1) Carthage has support for Static Frameworks. 
To indicate that one of the aliases is a Static Framework, modify the `repositoryMap` like so:

```yaml
repositoryMap:
- Framework:
  - name: t1
    type: static
  - name: t2
```

If left unspecified, an alias is a __Dynamic Framework by default__.

#### Platforms

Since version [0.17.1.49](https://github.com/blender/Rome/releases/tag/v0.17.1.49) Rome allows you
to specify what platforms are supported for a specific `Romefile Entry`. This serves a differet purpose
than the command line option `--platforms`.

```yaml
repositoryMap:
- Framework: 
  - name: t1
    type: static
    platforms: [iOS, Mac]
  - name: t2
```

The above means that `t1` is only available for `iOS` and `Mac`.
The `--platforms` command line options can be used to futher limit the Rome command to a
specific subset of the supported platfroms.


### Cache Structure

The following describes the structure of the cache that Rome creates and manages.

By default frameworks, dSYMs and .bcsymbolmaps are placed in the cache (local and/or remote)
according to the following convention:

```
<git-repository-name>/<platform>/<framework-name>.framework(.dSYM)-(static-)<version-hash>.zip
<git-repository-name>/<platform>/<bcsymbolmap-hash>.bcsymbolmap-(static-)<version-hash>.zip

```

[Carthage version files](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
are placed at:

```
<git-repository-name>/.<framework-name>.version-(static-)<version-hash>
```

For example the cache for the `Cartfile.resolved` in [RepositoryMap](#repositorymap)
would look like the following

```
/Users/blender/Library/Caches/Rome/
├── HockeySDK-iOS
│   └── iOS
│       ├── HockeySDK.framework-3.8.6.zip
│       ├── HockeySDK.framework.dSYM-3.8.6.zip
│       └── D034377A-B469-3819-97A7-1DC0AA293AC3.bcsymbolmap
├── awesome-framework-for-cat-names
│		├── iOS
│		│   ├── CatFramework.framework-883eea474e3932607988d4e74bf50c9799bfd99a.zip
│		│   └── CatFramework.framework.dSYM-883eea474e3932607988d4e74bf50c9799bfd99a.zip
│		├── tvOS
│		│   ├── CatFramework.framework-883eea474e3932607988d4e74bf50c9799bfd99a.zip
│		│   └── CatFramework.framework.dSYM-883eea474e3932607988d4e74bf50c9799bfd99a.zip
│		└── .CatFramework.version-883eea474e3932607988d4e74bf50c9799bfd99a
└─── better-dog-names
		├── iOS
		│   ├── DogFramework.framework-v4.0.0.zip
		│   └── DogFramework.framework.dSYM-v4.0.0.zip
		├── Mac
		│   ├── DogFramework.framework-v4.0.0.zip
		│   └── DogFramework.framework.dSYM-v4.0.0.zip
		└── .DogFramework.version-v4.0.0
```
#### Cache Prefix

Since version `0.12.0.31` Rome supports prefixes for top level directories in your
caches. You can append `--cache-prefix MY_PREFIX` to all commands.
This simply means that both the framework/dSYM and .version file conventional
locations can be prefixed by another directory of your choosing. Thus the conventions
become:

```
<MY_PREFIX>/<git-repository-name>/<platform>/<framework-name>.framework(.dSYM)-<version-hash>.zip
```

and

```
<MY_PREFIX>/<git-repository-name>/.<framework-name>.version-<version-hash>
```

This is particularly useful when the need to cache frameworks at the same version
but build with different versions of the compiler arises.

Suppose you want to cache v4.0.0 of `DogFramework` build for
Swift2.1/Swift3.1/Swift3.2/Swift4. Once built you can upload each build with the same
version number to a separate top level directory in the cache via the `--cache-prefix`
option.

Thus running for the Swift2.1 build

```
$ rome upload better-dog-names --platform iOS # note there is no prefix here
```

and running for the the Swift3.2 build

```
$ rome upload better-dog-names --platform iOS --cache-prefix Swift_3_2
```

would lead to the following cache structure

```
/Users/blender/Library/Caches/Rome/
├── better-dog-names
│   ├── iOS
│   │   ├── DogFramework.framework-v4.0.0.zip
│   │   └── DogFramework.framework.dSYM-v4.0.0.zip
│   └── .DogFramework.version-v4.0.0-iOS
└── Swift_3_2
    └── better-dog-names
        ├── iOS
        │   ├── DogFramework.framework-v4.0.0.zip
        │   └── DogFramework.framework.dSYM-v4.0.0.zip
        └── .DogFramework.version-v4.0.0-iOS
```

## Usage

Getting help:

```bash
$ rome --help
Cache tool for Carthage

Usage: rome COMMAND [-v]

Available options:
  -h,--help                Show this help text
  --version                Prints the version information
  -v                       Show verbose output

Available commands:
  upload                   Uploads frameworks and dSYMs contained in the local
                           Carthage/Build/<platform> to S3, according to the
                           local Cartfile.resolved
  download                 Downloads and unpacks in Carthage/Build/<platform>
                           frameworks and dSYMs found in S3, according to the
                           local Cartfile.resolved
  list                     Lists frameworks in the cache and reports cache
                           misses/hits, according to the local
                           Cartfile.resolved. Ignores dSYMs.
  utils                    A series of utilities to make life easier. `rome
                           utils --help` to know more
```

### Uploading

Uploading one or more frameworks, corresponding dSYMs, .bcsymbolmaps and [Carthage version files](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
if present (an empty list of frameworks will upload all frameworks found in `Cartfile.resolved`):

Referring to the `Cartfile.resolved` in [RepositoryMap](#repositorymap)

```bash
$ rome upload Alamofire
Uploaded Alamofire to: Alamofire/iOS/Alamofire.framework-4.3.0.zip
Uploaded Alamofire.dSYM to: Alamofire/iOS/Alamofire.framework.dSYM-4.3.0.zip
Uploaded Alamofire to: Alamofire/tvOS/Alamofire.framework-4.3.0.zip
Uploaded Alamofire.dSYM to: Alamofire/tvOS/Alamofire.framework.dSYM-4.3.0.zip
Uploaded Alamofire to: Alamofire/watchOS/Alamofire.framework-4.3.0.zip
Uploaded Alamofire.dSYM to: Alamofire/watchOS/Alamofire.framework.dSYM-4.3.0.zip
```

Uploading for a specific platform (all platforms are uploaded by default):

```bash
$ rome upload --platform ios Alamofire
Uploaded Alamofire to: Alamofire/iOS/Alamofire.framework-4.3.0.zip
Uploaded Alamofire.dSYM to: Alamofire/iOS/Alamofire.framework.dSYM-4.3.0.zip
```

If a local cache is specified in your `Romefile` and you wish to ignore it pass `--skip-local-cache` on the command line.

### Downloading

Downloading one or more frameworks, corresponding dSYMs, .bcsymbolmaps and
[Carthage version files](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
if present
(an empty list of frameworks will download all frameworks found in `Cartfile.resolved`):

Referring to the `Cartfile.resolved` in [RepositoryMap](#repositorymap)

```bash
$ rome download Alamofire
Downloaded Alamofire from: Alamofire/iOS/Alamofire.framework-4.3.0.zip
Downloaded Alamofire.dSYM from: Alamofire/iOS/Alamofire.framework.dSYM-4.3.0.zip
Error downloading Alamofire : The specified key does not exist.
Error downloading Alamofire.dSYM : The specified key does not exist.
Downloaded Alamofire from: Alamofire/tvOS/Alamofire.framework-4.3.0.zip
Downloaded Alamofire.dSYM from: Alamofire/tvOS/Alamofire.framework.dSYM-4.3.0.zip
Downloaded Alamofire from: Alamofire/watchOS/Alamofire.framework-4.3.0.zip
Downloaded Alamofire.dSYM from: Alamofire/watchOS/Alamofire.framework.dSYM-4.3.0.zip
```

Downloading for a specific platform (all platforms are downloaded by default):

```bash
$ rome download --platform ios,watchos Alamofire
Downloaded Alamofire from: Alamofire/iOS/Alamofire.framework-4.3.0.zip
Downloaded Alamofire.dSYM from: Alamofire/iOS/Alamofire.framework.dSYM-4.3.0.zip
Downloaded Alamofire from: Alamofire/watchOS/Alamofire.framework-4.3.0.zip
Downloaded Alamofire.dSYM from: Alamofire/watchOS/Alamofire.framework.dSYM-4.3.0.zip
```

If a local cache is specified in your `Romefile` and you wish to ignore it pass `--skip-local-cache` on the command line.

### Listing

Listing frameworks and reporting on their availability:

```bash
$ rome list
Alamofire 4.3.0 : +iOS -macOS +tvOS +watchOS
ResearchKit 1.4.1 : +iOS -macOS -tvOS -watchOS
```

Listing only frameworks present in the cache:

```bash
$ rome list --present
Alamofire 4.3.0 : +iOS +tvOS +watchOS
ResearchKit 1.4.1 : +iOS
```

Listing only frameworks missing from the cache:

```bash
$ rome list --missing
Alamofire 4.3.0 : -macOS
ResearchKit 1.4.1 : -macOS -tvOS -watchOS
```

Listing frameworks missing for specific platforms:

```bash
$ rome list --missing --platform watchos,tvos
ResearchKit 1.4.1 : -tvOS -watchOS
```

Forwarding a list of missing frameworks to Carthage for building:

```bash
$ rome list --missing --platform ios | awk '{print $1}' | xargs carthage build --platform ios
*** xcodebuild output can be found in ...
```

Since version `0.13.0.33` list results can also be printed as JSON by specifying `--print-format=JSON`

Note: `list` __completely ignores dSYMs, bcsymbolmap and Carthage version files__. If a dSYM
or a [Carthage version file](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
is missing, __the corresponding framework is still reported as present__.

### Utils

A collection of utilities to make life easier.

### migrate-romefile

Migrate the Romefile from INI to YAML __in place__, by running:

`rome ultils migrate-romefile`


## Troubleshooting & FAQ

### Getting "Image not found" when running an application using binaries

Implicit dependencies of frameworks when using binaries are not copied over by Xcode automatically despite "Always Embed Standard Libraries" set to **YES** (see [56](/blender/Rome/issues/56)).

Here is an example with ReactiveCocoa, which depends on CoreLocation and MapKit. If ReactiveCocoa is built via Carthage or as a Xcode subproject, CoreLocation and MapKit are copied into the app's bundle. On the other hand, when using the binary, Xcode has no clue of that and does not copy the necessary frameworks even if "Always Embed Standard Libraries" is set to yes.

To fix that, add an explicit import statement to one of your files:

```swift
// Implicit ReactiveCocoa Dependencies

import CoreLocation
import MapKit
```

### Supporting multiple Swift Versions

Storing artifacts or a the same famework at different Swift versions can be
achieved by specifying a cache prefix when using any Rome command like so:

```
$ rome upload --platform iOS --cache-prefix Swift3 Alamofire
$ rome download --platform iOS --cache-prefix Swift3 Alamofire
$ rome list --platform iOS --cache-prefix Swift3
```

If you prefer a more accurate way of generating cache prefixes for different swift versions
consider using the following:

```
--cache-prefix `xcrun swift --version | head -1 | sed 's/.*\((.*)\).*/\1/' | tr -d "()" | tr " " "-"`
```

The specified prefix is prepended to the git repository name in the caches.
Using a local cache path like `~/Library/Caches/Rome` will store Alamofire from
the example above at `~/Library/Caches/Rome/Swift3/Alamofire`

See [Cache Structure](#cache-structure) and [Cache Prefix](#cache-prefix)
for an in depth explanation.

## Developing

1. Install [Stack](https://github.com/commercialhaskell/stack) via homebrew `brew install stack`
1. Clone the repo `git clone https://github.com/blender/Rome.git`
1. `cd Rome && stack build`
1. Optional: Install brittany via `stack install brittany`
1. Optional: Install hlint via `stack install hlint`

### IDE

1. Optional: If you use VIM install [haskell-vim-how](https://github.com/begriffs/haskell-vim-now)
1. Optional: If you use [Visual Studio Code](https://code.visualstudio.com/) install [Haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)

## Releasing

1. Increase the version number in [Rome.cabal](https://github.com/blender/Rome/blob/master/Rome.cabal#L2) 
1. Increase the version number in [app/Main.hs](https://github.com/blender/Rome/blob/master/app/Main.hs#L13)
1. Increase the version number in [Rome.podspec](https://github.com/blender/Rome/blob/master/Rome.podspec#L3)
1. Commit
1. Create a [new pre-release](https://github.com/blender/Rome/releases) on Github
1. Attach the zipped binary
1. Promote to release
1. Run `pod trunk push Rome.podspec`
1. [Update the homebrew formula](https://github.com/blender/homebrew-tap)

## Presentations and Tutorials

Video tutorial on Rome given at [CocoaHeads Berlin](http://cocoaheads-berlin.org/) and [slides](https://speakerdeck.com/blender/caching-a-simple-solution-to-speeding-up-build-times)

- [Features](https://youtu.be/2cCIuidT9VA?t=387)
- [Usage](https://youtu.be/2cCIuidT9VA?t=600)
- [Romefile](https://youtu.be/2cCIuidT9VA?t=1104)

[![cocoaheads-berlin-video-presentation](http://i.imgur.com/1vC8jYq.png)](https://www.youtube.com/watch?v=2cCIuidT9VA)

[AppUnite article](https://appunite.com/blog/dependencies-ios-carthage) - comparison of popular approaches to building dependencies with Carthage by Szymon Mrozek 

## License
Rome is released under MIT License

Logo courtesy of [TeddyBear[Picnic]](http://www.freedigitalphotos.net/images/view_photog.php?photogid=3407) at FreeDigitalPhotos.net
