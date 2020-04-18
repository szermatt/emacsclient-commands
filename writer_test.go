package emacsclient

import (
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestWriteUnquoted(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: "123"}
	responses <- Response{
		Type: SuccessResponse,
		Text: "t"}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello"`}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hell`}
	responses <- Response{
		Type: ContinueResponse,
		Text: `o"`}
	close(responses)

	out := &strings.Builder{}
	assert.Nil(t, WriteUnquoted(responses, out))

	assert.Equal(t, "123\nt\nhello\nhello\n", out.String())
}

func TestWriteUnquotedWithError(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello"`}
	responses <- Response{
		Type: ErrorResponse,
		Text: "Some Error"}
	close(responses)

	out := &strings.Builder{}
	assert.Error(t, WriteUnquoted(responses, out))
	assert.Equal(t, "hello\n", out.String())
}

func TestWriteAll(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: "123"}
	responses <- Response{
		Type: SuccessResponse,
		Text: "t"}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello"`}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hell`}
	responses <- Response{
		Type: ContinueResponse,
		Text: `o"`}
	close(responses)

	out := &strings.Builder{}
	assert.Nil(t, WriteAll(responses, out))

	assert.Equal(t, "123\nt\n\"hello\"\n\"hello\"\n", out.String())
}

func TestWriteAllWithError(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: "t"}
	responses <- Response{
		Type: ErrorResponse,
		Text: "Some Error"}
	close(responses)

	out := &strings.Builder{}
	assert.Error(t, WriteAll(responses, out))
	assert.Equal(t, "t\n", out.String())
}

func TestConsumeAll(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hell`}
	responses <- Response{
		Type: ContinueResponse,
		Text: `o"`}
	close(responses)

	assert.Nil(t, ConsumeAll(responses))
}

func TestConsumeAllWithError(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: SuccessResponse,
		Text: "t"}
	responses <- Response{
		Type: ErrorResponse,
		Text: "Some Error"}
	close(responses)

	assert.Error(t, ConsumeAll(responses))
}

func TestWriteError(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: ErrorResponse,
		Text: "Some Error"}
	close(responses)

	err := ConsumeAll(responses)
	out := &strings.Builder{}
	WriteError(err, out)
	assert.Equal(t, "*ERROR*: Some Error\n", out.String())
}
