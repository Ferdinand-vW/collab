all: setup build client-compile

setup: client-setup server-setup

build: client-build server-build

client-setup:
	(cd client ; bower i)

client-build:
	(cd client ; pulp build)

client-compile:
	(cd client ; pulp build -O --to ../static/room/main.js)

server-setup:
	stack setup

server-build:
	stack build

server-run:
	stack exec collab-exe

clean: clean-client

clean-client:
	(cd client ; rm -rf output ; rm -rf .pulp-cache ; rm -rf .psci_modules ; rm -rf bower_components)
