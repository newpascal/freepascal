program dbtestframework;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  fpcunit, testregistry, testreport,
  toolsunit, DBResultsWriter,
  testbasics, testsqlfieldtypes, testdbbasics;
  
var
  FXMLResultsWriter: TXMLResultsWriter;
  FDBResultsWriter: TDBResultsWriter;
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  FXMLResultsWriter := TXMLResultsWriter.Create;
  FDBResultsWriter := TDBResultsWriter.Create;
  try
    testResult.AddListener(FXMLResultsWriter);
//    testResult.AddListener(FDBResultsWriter);
    FXMLResultsWriter.WriteHeader;
    GetTestRegistry.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally
    testResult.Free;
    FXMLResultsWriter.Free;
  end;
end.
