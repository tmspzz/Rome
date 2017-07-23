![](logo/colosseum.jpg)

# Rome [![rome-latest](https://img.shields.io/badge/release-v0.12.0.31-blue.svg)](https://github.com/blender/Rome/releases/tag/v0.12.0.31) ![total-downloads](https://img.shields.io/github/downloads/blender/Rome/total.svg) [![fastlane-plugin -badge](https://rawcdn.githack.com/fastlane/fastlane/master/fastlane/assets/plugin-badge.svg)](https://github.com/netbe/fastlane-plugin-rome) [![twitter-follow](https://img.shields.io/twitter/follow/tmpz.svg?style=social&label=Follow)](https://twitter.com/tmpz)


Rome is a tool that allows developers on Apple platforms to use:

- Amazon's S3
- or/and a local folder

as a shared cache for frameworks built with [Carthage](https://github.com/Carthage/Carthage).

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
	- [Romefile](#romefile)
		- [Cache section](#cache-section)
		- [RepositoryMap](#repositorymap)
		- [IgnoreMap](#ignoremap)
			- [Multiple Aliases](#multiple-aliases)
	- [Cache Structure](#cache-structure)
		- [Cache Prefix](#cache-prefix)
- [Usage](#usage)
	- [Uploading](#uploading)
	- [Downloading](#downloading)
	- [Listing](#listing)
- [Troubleshooting & FAQ](#troubleshooting--faq)
	- [Getting "Image not found" when running an application using binaries](#getting-image-not-found-when-running-an-application-using-binaries)
	- [Supporting multiple Swift Versions](#supporting-multiple-swift-versions)
- [Presentations and Tutorials](#presentations-and-tutorials)
- [Who uses Rome?](#who-uses-rome)
- [License](#license)

## Get Rome
`$ brew install blender/homebrew-tap/rome`

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
same level where the `Cartfile` is.

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

### Romefile

The Romefile has three purposes:

1. Specifies what caches to use - `[Cache]` section. This section is __required__.
1. Allows to use custom name mappings between repository names and framework names - `[RepositoryMap]` section. This section is __optional__ and can be omitted.
1. Allows to ignore certain framework names - `[IgnoreMap]` section. This section is __optional__ and can be omitted.

A Romefile looks like this:

```
[Cache]
  S3-Bucket = ios-dev-bucket
  local = ~/Library/Caches/Rome/

[RepositoryMap]
  HockeySDK-iOS = HockeySDK
  awesome-framework-for-cat-names = CatFramework
  better-dog-names = DogFramework

[IgnoreMap]
  xcconfigs = xcconfigs
```  

The Romefile is in the [INI format](https://en.wikipedia.org/wiki/INI_file)

#### Cache section
This section must contain __at least one__ between:
- the name of the S3 Bucket to upload/download to/from. The key `S3-Bucket` is __optional__ since Rome `0.11.0.x`.
- the path to local directory to use as an additional cache. The key `local` is __optional__.

#### RepositoryMap
This contains the mappings of git repository names with framework names.
This is particularly useful in case you are not using github and the "Organization/FrameworkName" convention.

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

simply add a `[RepositoryMap]` section to your `Romefile` and specify the following mapping:

```
[Cache]
  S3-Bucket = ios-dev-bucket

[RepositoryMap]
  HockeySDK-iOS = HockeySDK
  awesome-framework-for-cat-names = CatFramework
  better-dog-names = DogFramework
```

#### IgnoreMap
This contains the mappings of git repository names and framework names should be ignored.
This is particularly useful in case not all your `Cartfile.resolved` entries produce a framework.

Some repositories use Carthage as a simple mechanism to include other git repositories that do not produce frameworks.
Even Carthage itself does this, to include xcconfigs.


Example:

Suppose you have the following in your `Cartfile`

```
github "Quick/Nimble"
github "jspahrsummers/xcconfigs"
```

`xcconfigs` can be ignored by Rome by adding an `IgnoreMap` section in the Romefile


```
[IgnoreMap]
  xcconfigs = xcconfigs
```

##### Multiple Aliases

Since version `0.6.0.10` Rome supports multiple aliases for one map entry.
Suppose you have a framework `Framework` that builds two targets, `t1` and `t2`,
Rome can handle both targets by specifying

```
[RepositoryMap]
  Framework = t1, t2
```

If __ANY__ of the aliases is missing on S3, the entire entry will be reported as missing
when running `rome list [--missing]`

Multiple aliases are supported in `[IgnoreMap]` too

### Cache Structure

The following describes the structure of the cache that Rome creates and manages.

By default frameworks and dSYMs are placed in the cache (local and/or remote)
according to the following convention:

```
<git-repository-name>/<platform>/<framework-name>.framework(.dSYM)-<version-hash>.zip
```

[Carthage version files](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
are placed at:

```
<git-repository-name>/.<framework-name>.version-<version-hash>
```

For example the cache for the `Cartfile.resolved` in [RepositoryMap](#repositorymap)
would look like the following

```
/Users/blender/Library/Caches/Rome/
├── HockeySDK-iOS
│   └── iOS
│       ├── HockeySDK.framework-3.8.6.zip
│       └── HockeySDK.framework.dSYM-3.8.6.zip
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
S3 cache tool for Carthage

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
                           local Carftfile.resolved
  list                     Lists frameworks in the cache and reports cache
                           misses/hits, according to the local
                           Carftfile.resolved. Ignores dSYMs.
```

### Uploading

Uploading one or more frameworks, corresponding dSYMs and [Carthage version files](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
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

Downloading one or more frameworks, corresponding dSYMs and
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

Note: `list` __completely ignores dSYMs and Carthage version files__. If a dSYM
or a [Carthage version file](https://github.com/Carthage/Carthage/blob/master/Documentation/VersionFile.md)
is missing, __the corresponding framework is still reported as present__.

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

The specified prefix is prepended to the git repository name in the caches.
Using a local cache path like `~/Library/Caches/Rome` will store Alamofire from
the example above at `~/Library/Caches/Rome/Swift3/Alamofire`

See [Cache Structure](#cache-structure) and [Cache Prefix](#cache-prefix)
for an in depth explanation.

## Presentations and Tutorials

Video tutorial on Rome given at [CocoaHeads Berlin](http://cocoaheads-berlin.org/) and [slides](https://speakerdeck.com/blender/caching-a-simple-solution-to-speeding-up-build-times)

- [Features](https://youtu.be/2cCIuidT9VA?t=387)
- [Usage](https://youtu.be/2cCIuidT9VA?t=600)
- [Romefile](https://youtu.be/2cCIuidT9VA?t=1104)

[![cocoaheads-berlin-video-presentation](http://i.imgur.com/1vC8jYq.png)](https://www.youtube.com/watch?v=2cCIuidT9VA)

## Who uses Rome?

- https://www.sharecare.com/

## License
Rome is released under MIT License

Logo courtesy of [TeddyBear[Picnic]](http://www.freedigitalphotos.net/images/view_photog.php?photogid=3407) at FreeDigitalPhotos.net
