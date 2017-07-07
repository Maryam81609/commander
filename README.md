# Commander: A Runtime Verification Tool for Antidote Applications
Commander explores all possible behaviors in a written test scenario by the developer to check the specified invariants in Antidote [1] applications.
Commander has two phases: *recorder* and *replayer*. It first runs the test case and records all calls to the SUT API functions in addition to delivering those updates' logs to remote data centers. Either of a call to the SUT API function or delivering its log to a remote data center is referred as an event. Then, it explores all possible behaviors of the specified test scenario by reordering the recorded trace of events. Commander employs two schedulers: *delay* and *random* schedulers. Developers can tell the Commander which scheduler to use. For more detail, refer to [2].

## How to write a test case
### Create a new test case
To create an empty test case automatically, run: 

```bash
cd COMMANDER_ROOT
./commander.sh -n TESTNAME
```

Then, an empty test case with name `TESTNAME_comm.erl` will be creates under `COMMANDER_ROOT/comm_tests/` subdirectory. To check how it would be look like, see [`comm_test_template.erl`](https://github.com/Maryam81609/commander/blob/check_realistic_benchmarks/comm_test_template.erl). Now, you can edit the file and call the API functions of the Software Under Test (SUT).

### Test Case Header
```Erlang
-module(TESTNAME_comm).

-behavior(comm_test).

-include_lib("eunit/include/eunit.hrl").

%% comm_test callbacks
-export([check/1,
    handle_event/1,
    handle_object_invariant/2]).
```

Every test case must implement `comm_test` behavior. This behavior provides two interface functions `objects/2`, `event/2` and three callback functions `check/1`, `handle_event/1`, `handle_object_invariant/2`.

### Interface Functions
* `objects(?MODULE, InvariantArgs)` takes the test module and a list of required arguments as input. Argument `InvariantArgs` consists of data which is not modified during the execution such as objects or constant values. Later, Commander will call `handle_object_invariant/2` either after performing an API call or delivering an update log to a remote data center.
* `event(?MODULE, [EventNo, Node, CausalClock, AppArgs])` wraps around any call to  the SUT API. This function takes the test module and a list of four arguments as input. The arguments `?MODULE`, `EventNo`, `Node` and `CausalClock` are necessary for Commander to replay the corresponding event. The argument `EventNo` is a positive integer that is used to distinguish different events, and `Node` specifies the node on which this event is taking place, and `CausalClock` is a vector clock which specifies the dependency clock for the current event. The last argument `AppArgs` is a list of required arguments to pass to the SUT API function. 

### Callback Functions
* `handle_object_invariant(Node, InvariantArgs)` is called by Commander, and the `InvariantArgs` is the same argument passed to the interface function `objects/2`. The argument `Node` is the node on which the latest update has been done or delivered. This function always returns `true`.
* `handle_event([EventNo, Node, CausalClock, AppArgs])` is called by Commander in the replay phase, using the same arguments already passed to `event/2`.
* `check/1` is the entry point of the test module in the record phase. This function always returns `pass`.

## Run Commander for the given test case
To verify the SUT using the specified test case, run:

```bash
./commander.sh SCHEDULER SCHEDULER_PARAM TESTNAME
```
* `SCHEDULER` specifies the type of scheduler to use, and it is either `delay` or `random`
* `SCHEDULER_PARAM` for the scheduler `delay` specifies the delay bound which is a non negative integer. For the scheduler random, it specifies a seed. The following shows two examples for both schedulers:
	* For `delay` scheduler with delay bound `1`, run:
	
		```bash
		./commander.sh delay 1 TESTNAME
		```
		
	* For `random` scheduler with seed `{101, 202, 303}`, run:
	
		```bash
		./commander.sh random "{101, 202, 303}" TESTNAME
		```
* `TESTNAME` is the same name you used when creating a new test case.


## References
[1] [Antidote key-value data store](https://github.com/SyncFree/antidote)

