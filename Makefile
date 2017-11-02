all: setup server-build client-compile

setup: client-setup server-setup

build: client-build server-build

client: client-setup client-build client-compile

client-setup:
	(cd client ; bower i)

client-build:
	(cd client ; pulp build)

client-compile:
	( cd client ; pulp build --to ../static/index/main.js ; cp -r output/ ../)
	cp static/index/main.js static/room/main.js
	cp static/index/main.js static/register/main.js
	cp -r static/css/bootstrap/ static/index
	cp -r static/css/bootstrap/ static/room
	cp -r static/css/bootstrap/ static/register
	cp static/css/style.css static/index
	cp static/css/style.css static/room
	cp static/css/style.css static/register

client-psci:
	(cd client ; pulp psci)

server: server-setup server-build

server-setup:
	stack setup

server-build:
	stack build

server-run:
	stack exec collab-exe

clean: clean-client

clean-client:
	(cd client ; rm -rf output ; rm -rf .pulp-cache ; rm -rf .psci_modules ; rm -rf bower_components)