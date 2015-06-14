
[![Build Status](https://secure.travis-ci.org/2600hz/kazoo.png)](http://travis-ci.org/2600hz/kazoo)

## Kazoo

### Cloud-Based Telecommunications For Everyone

Welcome to Kazoo, an ambitious project to bring cloud-based VoIP and telecommunications to everyone. Our goal is to provide the world with a free, open telecommunications software platform. Released under the [OSI-approved MPL 1.1 open source software license](http://opensource.org/licenses/MPL-1.1), we're building upon strong FOSS components like [GNU](https://www.gnu.org/)/[Linux](https://en.wikipedia.org/wiki/Linux), [Erlang](http://erlang.org), [FreeSWITCH](https://freeswitch.org), [CouchDB](https://couchdb.apache.org) \(specifically [BigCouch](http://bigcouch.cloudant.com)), and [RabbitMQ](https://www.rabbitmq.com). Our project is a great example of the wonderful things that can happen when software is open.

### What is Kazoo?

Kazoo is an API-based platform that lets you use your existing phones,
programming languages and IT skills to build voice, video and SMS services. We
focus on building a simple, powerful communications platform and let you focus
on marketing, servicing and integrating communications with your clients
systems.

Dubbed a "scalable, distributed, cloud-based" telephony platform, Kazoo
provides simple to use, consistent API so that people who know little about
building telecom systems can still dive in and utilize the power of the phone
network. In practical terms that means you can create your own phone service
in the cloud. Or on a server in your mother's basement. Or in a distributed
cluster of servers around the globe. It's up to you.


## Who Can Contribute?

_Everyone can!_

### Not a coder?

No problem!  We have many different places where folks can chip in time. These include (but aren't limited to):

* Maintaining and expanding the documenation
* Blogging, tweeting, and other forms of publicizing your usage of and love for the platform, community, code, company, etc.
* Join us in real time and help other community members:
    * on IRC (#2600hz on Freenode)
    * our [Users mailing list](https://groups.google.com/forum/?fromgroups#!forum/2600hz-users)
    * our [Developers mailing list](https://groups.google.com/forum/?fromgroups#!forum/2600hz-dev)
    * our [Forum](http://forum.2600hz.com/)
* Contribute scripts to the [community repo](https://github.com/2600hz/community-scripts)
* Help with building RPMs, DEBs, and other system packages
* Browse the open [Issues](https://2600hz.atlassian.net/browse/KAZOO) and test bugs to see if they're valid
* Make a sugestion!

### Internationalization Help

Translation efforts are always appreciated. We're working on redesigning how our back- and front-ends handle the world's languages and hope to make it easy to contribute translations. Stay tuned for more; please let us know of your interest and be sure to join the mailing lists so you can receive announcements regarding internationalization efforts.

If you have a non-US deployment, please consider sharing your system configuration!  We are trying to build [examples](https://github.com/2600hz/kazoo/tree/master/applications/crossbar/doc/internationalization) for every locality and fix any deficiencies.

### I know me some Erlang! How can I contribute?

* Clone the repo
    * If you're completely new to Git, read Github's [great introduction](https://help.github.com/articles/fork-a-repo) to forking repos
* Squash a bug, build a feature, etc
    * Browse the [ticket queue](https://2600hz.atlassian.net/browse/KAZOO) and find one you'd like to tackle.
    * Feel free to drop a line to the [dev mailing list](https://groups.google.com/forum/?fromgroups#!forum/2600hz-dev) letting us know you're taking on a ticket, especially if you have questions.
* Once you feel the code is ready for inclusion, issue a [pull request](https://help.github.com/articles/using-pull-requests)!
    * Please keep your pull requests focused and specific to the task at hand
    * If, in the course of your work you add/modify/delete code unrelated to the core task, create a new ticket describing why you've made these additional changes, and issue a separate pull request.

### Do you know other languages?

* Help us write some automated testing tools
* Check out our Javascript/HTML/CSS [GUI](https://github.com/2600hz/kazoo_ui)
* Check out our REST APIs and help us create SDKs in your favorite languages ([PHP](https://github.com/2600hz/kazoo-php-sdk) | [RUBY](https://github.com/2600hz/kazoo-ruby-sdk))
* Check out our Pivot APIs and help us create SDKs in your favorite languages
* Check out our AMQP APIs and create a WhApp in your favorite language
* Help out with the [configuration scripts](https://github.com/2600hz/kazoo-configs)

## Build Dependencies

* erlang R15+
* python-simplejson
* libxslt
* gcc-c++
* zip
* unzip
* expat-devel
* zlib-devel
* openssl-devel
* libxml-devel
* make
* nc

