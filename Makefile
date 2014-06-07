REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

rel: compile
	@( $(REBAR) generate )

run:
	@( erl -pa ebin deps/*/ebin -s adserver )

.PHONY: all deps compile clean rel run