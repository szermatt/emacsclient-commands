package emacsclient

import (
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func TestAsChar(t *testing.T) {
	assert.Equal(t, "?a", AsChar("a"))
	assert.Equal(t, "?α", AsChar("α"))
	assert.Equal(t, "?\\ ", AsChar(" "))
	assert.Equal(t, "?\\\"", AsChar("\""))
	assert.Equal(t, "?\\[", AsChar("["))
	assert.Equal(t, "?\\]", AsChar("]"))
	assert.Equal(t, "?\\)", AsChar(")"))
	assert.Equal(t, "?\\(", AsChar("("))
	assert.Equal(t, "?\\n", AsChar("\n"))
	assert.Equal(t, "?\\x03", AsChar("\003"))
	assert.Equal(t, "?\\x00", AsChar(""))
}

func TestAsBool(t *testing.T) {
	assert.Equal(t, "t", AsBool(true))
	assert.Equal(t, "nil", AsBool(false))
}

func TestAsString(t *testing.T) {
	assert.Equal(t, `"string"`, AsString("string"))
	assert.Equal(t, `"αλπηα"`, AsString("αλπηα"))
	assert.Equal(t, `"\"hello\", he said"`, AsString("\"hello\", he said"))
	assert.Equal(t, `"the end.\n"`, AsString("the end.\n"))
}

func TestAsStringList(t *testing.T) {
	assert.Equal(t, `'("hello" "world")`, AsStringList([]string{"hello", "world"}))
	assert.Equal(t, `'("hello")`, AsStringList([]string{"hello"}))
	assert.Equal(t, `'()`, AsStringList([]string{}))
}

func TestReadBool(t *testing.T) {
	responses := make(chan Response, 100)

	responses <- Response{
		Type: SuccessResponse,
		Text: "nil"}
	result, err := ReadBool(responses)
	assert.Nil(t, err)
	assert.Equal(t, false, result)

	responses <- Response{
		Type: SuccessResponse,
		Text: "t"}
	result, err = ReadBool(responses)
	assert.Nil(t, err)
	assert.Equal(t, true, result)

	responses <- Response{
		Type: SuccessResponse,
		Text: "'()"}
	result, err = ReadBool(responses)
	assert.Nil(t, err)
	assert.Equal(t, true, result)

	responses <- Response{
		Type: SuccessResponse,
		Text: `"text"`}
	result, err = ReadBool(responses)
	assert.Nil(t, err)
	assert.Equal(t, true, result)
}

func TestReadBoolWithError(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: ErrorResponse,
		Text: "Something's wrong"}
	_, err := ReadBool(responses)
	assert.Error(t, err)
}

func TestReadBoolWithContinue(t *testing.T) {
	responses := make(chan Response, 100)
	responses <- Response{
		Type: ContinueResponse,
		Text: `more text"`}
	_, err := ReadBool(responses)
	assert.Error(t, err)
}

func TestReadSingleString(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello, world"`}
	assert.Nil(t, ReadString(responses, out))
	assert.Equal(t, "hello, world", out.String())
}

func TestReadStringWithEscapedCharacters(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"\"\a\b\t\n\v\f\r\e\s\d hello αλπηα"`}
	assert.Nil(t, ReadString(responses, out))
	assert.Equal(t, "\"\a\b\t\n\v\f\r\x1b \x7f hello αλπηα", out.String())
}

func TestReadContinuedString(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello, `}
	responses <- Response{
		Type: ContinueResponse,
		Text: `world"`}
	assert.Nil(t, ReadString(responses, out))
	assert.Equal(t, "hello, world", out.String())
}

func TestReadContinuedStringWithinEscapeSequence(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello\`}
	responses <- Response{
		Type: ContinueResponse,
		Text: `n"`}
	assert.Nil(t, ReadString(responses, out))
	assert.Equal(t, "hello\n", out.String())
}

func TestReadStringWithError(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: ErrorResponse,
		Text: "something went wrong"}
	assert.Error(t, ReadString(responses, out))
}

func TestReadStringContinuedWithError(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello\`}
	responses <- Response{
		Type: ErrorResponse,
		Text: "something went wrong"}
	assert.Error(t, ReadString(responses, out))
}

func TestReadStringNotFinished(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: `"hello\`}
	close(responses)

	assert.Error(t, ReadString(responses, out))
}

func TestReadStringNotAString(t *testing.T) {
	responses := make(chan Response, 100)
	out := &strings.Builder{}
	responses <- Response{
		Type: SuccessResponse,
		Text: "123"}
	close(responses)

	assert.Error(t, ReadString(responses, out))
}

func TestExecuteTemplate(t *testing.T) {
	type argType struct {
		AString     string
		AStringList []string
		AnEmptyList []string
		ABool       bool
		AChar       string
	}
	text, err := ExecuteTemplate(
		&argType{
			AString:     "foo bar",
			AStringList: []string{"foo", "bar"},
			AnEmptyList: []string{},
			ABool:       true,
			AChar:       "c",
		},
		`(astring {{str .AString}})
(astringlist {{strList .AStringList}})
(anemptylist {{strList .AnEmptyList}})
(abool {{bool .ABool}})
(achar {{char .AChar}})`)
	assert.Nil(t, err)
	assert.Equal(t, `(astring "foo bar")
(astringlist '("foo" "bar"))
(anemptylist '())
(abool t)
(achar ?c)`, text)
}
