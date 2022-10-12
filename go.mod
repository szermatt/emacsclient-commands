module github.com/szermatt/emacsclient

go 1.13

replace github.com/Microsoft/go-winio v0.4.15-0.20200113171025-3fe6c5262873 => github.com/endocrimes/go-winio v0.4.13-0.20190628114223-fb47a8b41948

require (
	github.com/alessio/shellescape v1.4.1 // indirect
	github.com/hashicorp/nomad v1.2.13
	github.com/pkg/errors v0.9.1
	github.com/stretchr/testify v1.8.0
	github.com/tudurom/ttyname v0.0.0-20180413195752-f68e701b3b18
	gopkg.in/alessio/shellescape.v1 v1.0.0-20170105083845-52074bc9df61
)
