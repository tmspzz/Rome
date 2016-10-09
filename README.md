# Rome

Rome is a tool that allows developers on Apple platforms to use Amazon's S3 as a
shared cache for frameworks built with [Carthage](https://github.com/Carthage/Carthage).

## The problem

Suppose you're working a number of frameworks for you iOS project and want to
share those with your team. A great way to do so is to use Carthage and
have team members point the `Cartfile` to the new framework version (or branch, tag, commit)
and run `carthage update`.

Unfortunately this will require them to build from scratch the new framework.
This is particularly annoying if the dependency tree for that framework is big
and / or takes a long time to build.

## The solution

Use a cache. The first team member can build the framework and share it while all
other developers can get it from the cache with no waiting time.

## Workflow

The Rome's workflow changes depending if you are the producer (i.e. the first
person in your team to build the framework) or the consumer.

### Producer workflow

```
$ vi Cartfile # point to the new version of the framework
$ carthage update && rome upload
```

### Consumer workflow

```
$ vi Cartfile # point to the new version of the framework
$ carthage update --no-build && rome download
```

or

```
$ vi Cartfile.resolved # point to the new version of the framework
$ rome download
```
## Set up and Usage

- First you need a `.aws/credentials` file in your home folder. This is used to specify
your AWS Credentials.
- Second you need a `.aws/config` file in your home folder. This is used to specify the AWS
region.
- Third you need a `Romefile` in the project where you want to use Rome. At the
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

The Romefile has two purposes:

1. Specifies what S3 bucket to use - [Cache] section. This section is __required__.
1. Allows to use custom name mappings between repository names and framework names - [RepositoryMap] section. This section is __optional__ and can be omitted.

A Romefile looks like this:

```
[Cache]
  S3-Bucket = ios-dev-bucket

[RepositoryMap]
  HockeySDK-iOS = HockeySDK
  awesome-framework-for-cat-names = CatFramework
  better-dog-names = DogFramework
```  

The Romefile is the [INI format](https://en.wikipedia.org/wiki/INI_file)

#### S3Bucket section
This section contains the name of the S3 bucket you want Rome to use to upload/download.

#### RepositoryMap
This contains the mappings of git repository names with framework names.
This is particularly useful inn case you are not using github and the "Organization/FrameworkName" convention.

Example:

Suppose you have the following in your `Cartfile`

```
github "bitstadium/HockeySDK-iOS" "3.8.6"
git "http://stash.myAnimalStartup.com/scm/iossdk/awesome-framework-for-cat-names.git" ~> 3.3.1
git "http://stash.myAnimalStartup.com/scm/iossdk/better-dog-names.git" ~> 0.4.4
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

##### RepositoryMap Multiple Aliases

Since version `0.6.0.10` Rome supports multiple aliases for one map entry.
Suppose you have a framework `Framework` that builds two targets, `t1` and `t2`,
Rome can handle both targets by specifying

```
[RepositoryMap]
  Framework = t1, t2
```

If __ANY__ of the aliases is missing on S3, the entire entry will be reported as missing
when running `rome list [--missing]`

### Usage

Getting help:

```
$ rome --help
S3 cache tool for Carthage

Usage: rome COMMAND [-v]

Available options:
  -h,--help                Show this help text
  --version                Prints the version information
  -v                       Show verbose output

Available commands:
  upload                   Uploads frameworks and dSYMs contained in the local
                           Carthage/Build/iOS to S3, according to the local
                           Cartfile.resolved
  download                 Downloads and unpacks in Carthage/Build/iOS
                           frameworks and dSYMs found in S3, according to the
                           local Carftfile.resolved
  list                     Lists frameworks in the cache and reports cache
                           misses/hits, according to the local
                           Carftfile.resolved. Ignores dSYMs.
```

#### Uploading

Uploading one or more frameworks and corresponding dSYMs
(an empty list of frameworks will upload all frameworks found in `Cartfile.resolved`):

Referring to the `Cartfile.resolved` in [RepositoryMap](#repositorymap)

```
$ rome upload HockeySDK-iOS awesome-framework-for-cat-names
Uploaded HockeySDK to: bitstadium/HockeySDK.framework-3.8.6.zip
Uploaded HockeySDK.dSYM to: bitstadium/HockeySDK.framework.dSYM-3.8.6.zip
Uploaded CatFramework to: CatFramework/CatFramework.framework-3.3.1.zip
Uploaded CatFramework.dSYM to: CatFramework/CatFramework.framework.dSYM-3.3.1.zip
```

#### Downloading

Downloading one or more frameworks and corresponding dSYMs
(an empty list of frameworks will download all frameworks found in `Cartfile.resolved`):

Referring to the `Cartfile.resolved` in [RepositoryMap](#repositorymap)

```
$ rome download HockeySDK-iOS awesome-framework-for-cat-names
Downloaded HockeySDK from: HockeySDK/HockeySDK.framework-3.8.6.zip
Unzipped HockeySDK from: HockeySDK.framework-3.8.6.zip
Downloaded HockeySDK.dSYM from: HockeySDK/HockeySDK.framework.dSYM-3.8.6.zip
Unzipped HockeySDK.dSYM from: HockeySDK.framework.dSYM-3.8.6.zip
Downloaded CatFramework from: CatFramework/CatFramework.framework-3.3.1.zip
Unzipped CatFramework from: CatFramework.framework-3.3.1.zip
Downloaded CatFramework from: CatFramework/CatFramework.framework.dSYM-3.3.1.zip
Unzipped CatFramework from: CatFramework.framework.dSYM-3.3.1.zip
```

#### Listing

Listing frameworks and reporting on their availability:
```
$ rome list
Alamofire 3.4.1 ✔︎
GCDKit 1.2.5 ✔︎
HanekeSwift v0.10.1 ✔︎
HockeySDK-iOS 3.8.6 ✔︎
KeychainAccess v2.3.6 ✔︎
M13Checkbox 2.1.2 ✔︎
ResearchKit 1.3.1 ✘
```

Listing only frameworks present in the cache:

```
$ rome list --present
Alamofire
GCDKit
HanekeSwift
HockeySDK-iOS
KeychainAccess
M13Checkbox
```

Listing only frameworks missing from the cache:

```
$ rome list --present
ResearchKit
```

Note: `list` __completely ignores dSYMs__. If a dSYM is missing the corresponding
framework is still reported as present.

## Get Rome
The Rome binary is attached as a zip to the [releases page](https://github.com/blender/Rome/releases) here on GitHub.
