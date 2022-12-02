arbeitsinspektor
=====

The _Arbeitsinspektor_ (Health and Safety Executive) makes sure workloads within your Kubernetes cluster abide 
the relevant _labour laws_. 

Usage
-----
_arbeitsinspektor_ can be installed using [helm](https://helm.sh).

```
$ helm repo add arbeitsinspektor https://fridayy.github.io/arbeitsinspektor
$ helm install arbeitsinspektor arbeitsinspektor/arbeitsinspektor -f values.yaml
```

Possible configuration values are documented in the [next section](#configuration).

Configuration
-----
tbd


Development:Build
-----

    $ rebar3 compile
