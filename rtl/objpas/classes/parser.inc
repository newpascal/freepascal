{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                             TParser                                      *}
{****************************************************************************}

const
  ParseBufSize     = 4096;

procedure TParser.ReadBuffer;
var
  Count            : Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);

  FSourceEnd[0] := FSaveChar;
  Count         := FBufPtr - FSourcePtr;
  if Count <> 0 then
  begin
    Move(FSourcePtr[0], FBuffer[0], Count);
  end;

  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));

  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if (FSourceEnd = FBufEnd) then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
    begin
      Error(SLineTooLong);
    end;
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TParser.SkipBlanks;
begin
  while FSourcePtr^ < #33 do begin
    if FSourcePtr^ = #0 then begin
      ReadBuffer;
      if FSourcePtr^ = #0 then exit;
      continue;
    end else if FSourcePtr^ = #10 then Inc(FSourceLine);
    Inc(FSourcePtr);
  end;
end;

constructor TParser.Create(Stream: TStream);
begin
  inherited Create;

  FStream := Stream;
  GetMem(FBuffer, ParseBufSize);

  FBuffer[0]  := #0;
  FBufPtr     := FBuffer;
  FBufEnd     := FBuffer + ParseBufSize;
  FSourcePtr  := FBuffer;
  FSourceEnd  := FBuffer;
  FTokenPtr   := FBuffer;
  FSourceLine := 1;

  NextToken;
end;


destructor TParser.Destroy;
begin
  if Assigned(FBuffer) then
  begin
    FStream.Seek(PtrUInt(FTokenPtr) - PtrUInt(FBufPtr), 1);
    FreeMem(FBuffer, ParseBufSize);
  end;

  inherited Destroy;
end;

procedure TParser.CheckToken(T : Char);
begin
  if Token <> T then
  begin
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      toString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
  end;
end;

procedure TParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then
    ErrorFmt(SSymbolExpected, [S]);
end;

Procedure TParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

Procedure TParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

Procedure TParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateFmt(SParseError, [Message, FSourceLine]);
end;


procedure TParser.HexToBinary(Stream: TStream);

  function HexDigitToInt(c: Char): Integer;
  begin
    if (c >= '0') and (c <= '9') then Result := Ord(c) - Ord('0')
    else if (c >= 'A') and (c <= 'F') then Result := Ord(c) - Ord('A') + 10
    else if (c >= 'a') and (c <= 'f') then Result := Ord(c) - Ord('a') + 10
    else Result := -1;
  end;

var
  buf: array[0..255] of Byte;
  digit1: Integer;
  bytes: Integer;
begin
  SkipBlanks;
  while FSourcePtr^ <> '}' do begin
    bytes := 0;
    while True do begin
      digit1 := HexDigitToInt(FSourcePtr[0]);
      if digit1 < 0 then break;
      buf[bytes] := digit1 shl 4 or HexDigitToInt(FSourcePtr[1]);
      Inc(FSourcePtr, 2);
      Inc(bytes);
    end;
    if bytes = 0 then Error(SInvalidBinary);
    Stream.Write(buf, bytes);
    SkipBlanks;
  end;
  NextToken;
end;


Function TParser.NextToken: Char;
var
  CharCount         : Integer;
 
  procedure PutChar(achar: Word);
  begin
    inc(CharCount);
    if length(fString) < CharCount then begin
     setlength(fString,length(fString) + length(fString) div 4 + 64);
    end;
    fString[CharCount]:= WideChar(achar);
  end;
 
var
  I : Int64;
  P : PChar;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        CharCount:= 0;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                PutChar(I)
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  PutChar(Word(P^));
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        setlength(fString,CharCount);
        Result := toString;
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := toInteger;
      end;
    '-', '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := toInteger;
        while (P^ in ['0'..'9', '.', 'e', 'E', '+', '-']) and not
          ((P[0] = '.') and not (P[1] in ['0'..'9', 'e', 'E'])) do
        begin
          Inc(P);
          Result := toFloat;
        end;
      end;
  else
    Result := P^;
    if Result <> toEOF then Inc(P);
  end;
  FSourcePtr := P;
  FToken := Result;
end;

Function TParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;


Function TParser.TokenComponentIdent: String;
var
  P                : PChar;
begin
  CheckToken(toSymbol);

  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

{$ifndef FPUNONE}
Function TParser.TokenFloat: Extended;
var
  I,FloatError       : Integer;
  Back             : Real;
  S                : String;
  
begin
  Result   := 0;
  S:=TokenString;
  // Convert , decimal separator to ., handle backwards compatibility streams.
  for I:=1 to Length(S) do
    if (S[i]=',') then
      S[i]:='.';
  Val(S, Back, FloatError);
  Result := Back;
end;
{$endif}

Function TParser.TokenInt: Int64;
begin
  Result := StrToInt64(TokenString);
end;

Function TParser.TokenString: string;
var
  L                : Integer;
begin
  if FToken = toString then begin
    result:= fString;
  end else begin
    L := FSourcePtr - FTokenPtr;
    SetLength(Result,L);
    If (L>0) then
      Move(FTokenPtr^,Result[1],L);
  end;
end;

Function TParser.TokenWideString: widestring;
begin
  if FToken = toString then
    result:= fString
  else
    result:= TokenString
end;


Function TParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and (CompareText(S, TokenString) = 0);
end;
