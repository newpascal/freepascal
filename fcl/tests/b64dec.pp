// $Id: b64dec.pp,v 1.4 2005/02/14 17:13:18 peter Exp $

// base64-decodes data from StdIn and writes the output to StdOut
// (c) 1999 Sebastian Guenther

{$MODE objfpc}

program b64dec;
uses classes, base64, sysutils;
var
  b64decoder: TBase64DecodingStream;
  InputStream: TStream;
  IsEnd: Boolean;
begin

  InputStream := THandleStream.Create(StdInputHandle);

  b64decoder := TBase64DecodingStream.Create(InputStream);

  IsEnd := False;
  while not IsEnd do
    try
      Write(Chr(b64decoder.ReadByte));
    except
      on e: EStreamError do IsEnd := True;
    end;

  b64decoder.Free;
  InputStream.Free;
end.


{
  $Log: b64dec.pp,v $
  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
