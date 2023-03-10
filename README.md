arbeitsinspektor
=====

The _Arbeitsinspektor_ (Health and Safety Executive) makes sure workloads within your Kubernetes cluster abide 
the relevant _labour laws_. 

What?
-----
_arbeitsinspektor_ is a simple kubernetes cron job that scales workloads based on time. For example some workloads should 
only run within the timeframe of `mon-fri[07:00-17:00]`. _arbeitsinspektor_ takes care of that. Definition are stored in a CRD
`businesshours`.

Example:

```
 definitions:
 - definition: mon-fri[07:00-17:00]
   label:
     k8s.some.company/environment: development
```
> All deployments with the label `k8s.some.company/environment=development` are only allowed to run between monday and friday from 07:00 UTC - 17:00 UTC. Outside of these times the affected workloads will be scaled to zero.


Alternatives
-----
Containers with `kubectl` installed and some shell magic may achieve comparable results. See [here](https://blog.mimacom.com/time-based-scaling-for-kubernetes/).

> At its current state _arbeitsinspektor_ does not provide any additional functionality compared to a hand-rolled solution as the one proposed. However this is subject to change.

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
