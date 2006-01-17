{
    This include file contains the variants
    support for FPC

    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef fpc}
{$mode objfpc}
{$endif}
{$h+}

{ Using inlining for small system functions/wrappers }
{$inline on}
{$define VARIANTINLINE}

unit variants;

interface

  uses
    sysutils,sysconst,rtlconsts,typinfo;

type
  EVariantParamNotFoundError = class(EVariantError);
  EVariantInvalidOpError = class(EVariantError);
  EVariantTypeCastError = class(EVariantError);
  EVariantOverflowError = class(EVariantError);
  EVariantInvalidArgError = class(EVariantError);
  EVariantBadVarTypeError = class(EVariantError);
  EVariantBadIndexError = class(EVariantError);
  EVariantArrayLockedError = class(EVariantError);
  EVariantNotAnArrayError = class(EVariantError);
  EVariantArrayCreateError = class(EVariantError);
  EVariantNotImplError = class(EVariantError);
  EVariantOutOfMemoryError = class(EVariantError);
  EVariantUnexpectedError = class(EVariantError);
  EVariantDispatchError = class(EVariantError);
  EVariantRangeCheckError = class(EVariantOverflowError);
  EVariantInvalidNullOpError = class(EVariantInvalidOpError);

  TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);
  TNullCompareRule = (ncrError, ncrStrict, ncrLoose);
  TBooleanToStringRule = (bsrAsIs, bsrLower, bsrUpper);

Const
  OrdinalVarTypes = [varSmallInt, varInteger, varBoolean, varShortInt,
                     varByte, varWord,varLongWord,varInt64];
  FloatVarTypes = [varSingle, varDouble, varCurrency];

{ Variant support procedures and functions }

function VarType(const V: Variant): TVarType;
function VarAsType(const V: Variant; AVarType: TVarType): Variant;
function VarIsType(const V: Variant; AVarType: TVarType): Boolean; overload;
function VarIsType(const V: Variant; const AVarTypes: array of TVarType): Boolean; overload;
function VarIsByRef(const V: Variant): Boolean;

function VarIsEmpty(const V: Variant): Boolean;
procedure VarCheckEmpty(const V: Variant);
function VarIsNull(const V: Variant): Boolean;
function VarIsClear(const V: Variant): Boolean;

function VarIsCustom(const V: Variant): Boolean;
function VarIsOrdinal(const V: Variant): Boolean;
function VarIsFloat(const V: Variant): Boolean;
function VarIsNumeric(const V: Variant): Boolean;
function VarIsStr(const V: Variant): Boolean;

function VarToStr(const V: Variant): string;
function VarToStrDef(const V: Variant; const ADefault: string): string;
function VarToWideStr(const V: Variant): WideString;
function VarToWideStrDef(const V: Variant; const ADefault: WideString): WideString;

function VarToDateTime(const V: Variant): TDateTime;
function VarFromDateTime(const DateTime: TDateTime): Variant;

function VarInRange(const AValue, AMin, AMax: Variant): Boolean;
function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant;

function VarSameValue(const A, B: Variant): Boolean;
function VarCompareValue(const A, B: Variant): TVariantRelationship;

function VarIsEmptyParam(const V: Variant): Boolean;

procedure VarClear(var V: Variant);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
procedure VarClear(var V: OleVariant);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}

procedure SetClearVarToEmptyParam(var V: TVarData);

function VarIsError(const V: Variant; out AResult: HRESULT): Boolean;
function VarIsError(const V: Variant): Boolean;
function VarAsError(AResult: HRESULT): Variant;

function VarSupports(const V: Variant; const IID: TGUID; out Intf): Boolean;
function VarSupports(const V: Variant; const IID: TGUID): Boolean;

{ Variant copy support }
procedure VarCopyNoInd(var Dest: Variant; const Source: Variant);

{ Variant array support procedures and functions }

function VarArrayCreate(const Bounds: array of SizeInt; AVarType: TVarType): Variant;
function VarArrayCreate(const Bounds: pvararrayboundarray; Dims : SizeInt; AVarType: TVarType): Variant;
function VarArrayOf(const Values: array of Variant): Variant;

function VarArrayAsPSafeArray(const A: Variant): PVarArray;

function VarArrayDimCount(const A: Variant) : SizeInt;
function VarArrayLowBound(const A: Variant; Dim : SizeInt) : SizeInt;
function VarArrayHighBound(const A: Variant; Dim : SizeInt) : SizeInt;

function VarArrayLock(const A: Variant): Pointer;
procedure VarArrayUnlock(const A: Variant);

function VarArrayRef(const A: Variant): Variant;

function VarIsArray(const A: Variant): Boolean;
function VarIsArray(const A: Variant; AResolveByRef: Boolean): Boolean;

function VarTypeIsValidArrayType(const AVarType: TVarType): Boolean;
function VarTypeIsValidElementType(const AVarType: TVarType): Boolean;

{ Variant <--> Dynamic Arrays }

procedure DynArrayToVariant(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer);
procedure DynArrayFromVariant(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer);

{ Global constants }

function Unassigned: Variant; // Unassigned standard constant
function Null: Variant;       // Null standard constant

var
  EmptyParam: OleVariant;

{ Custom variant base class }

type
  TVarCompareResult = (crLessThan, crEqual, crGreaterThan);
  TCustomVariantType = class(TObject, IInterface)
  private
    FVarType: TVarType;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure SimplisticClear(var V: TVarData);
    procedure SimplisticCopy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean = False);
    procedure RaiseInvalidOp;
    procedure RaiseCastError;
    procedure RaiseDispError;
    function LeftPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean; virtual;
    function RightPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean; virtual;
    function OlePromotion(const V: TVarData; out RequiredVarType: TVarType): Boolean; virtual;
    procedure DispInvoke(var Dest: TVarData; const Source: TVarData; CallDesc: PCallDesc; Params: Pointer); virtual;
    procedure VarDataInit(var Dest: TVarData);
    procedure VarDataClear(var Dest: TVarData);
    procedure VarDataCopy(var Dest: TVarData; const Source: TVarData);
    procedure VarDataCopyNoInd(var Dest: TVarData; const Source: TVarData);
    procedure VarDataCast(var Dest: TVarData; const Source: TVarData);
    procedure VarDataCastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); overload;
    procedure VarDataCastTo(var Dest: TVarData; const AVarType: TVarType); overload;
    procedure VarDataCastToOleStr(var Dest: TVarData);
    procedure VarDataFromStr(var V: TVarData; const Value: string);
    procedure VarDataFromOleStr(var V: TVarData; const Value: WideString);
    function VarDataToStr(const V: TVarData): string;
    function VarDataIsEmptyParam(const V: TVarData): Boolean;
    function VarDataIsByRef(const V: TVarData): Boolean;
    function VarDataIsArray(const V: TVarData): Boolean;
    function VarDataIsOrdinal(const V: TVarData): Boolean;
    function VarDataIsFloat(const V: TVarData): Boolean;
    function VarDataIsNumeric(const V: TVarData): Boolean;
    function VarDataIsStr(const V: TVarData): Boolean;
  public
    constructor Create; overload;
    constructor Create(RequestedVarType: TVarType); overload;
    destructor Destroy; override;
    function IsClear(const V: TVarData): Boolean; virtual;
    procedure Cast(var Dest: TVarData; const Source: TVarData); virtual;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); virtual;
    procedure CastToOle(var Dest: TVarData; const Source: TVarData); virtual;
    procedure Clear(var V: TVarData); virtual; abstract;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); virtual; abstract;
    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp); virtual;
    procedure UnaryOp(var Right: TVarData; const Operation: TVarOp); virtual;
    function CompareOp(const Left, Right: TVarData; const Operation: TVarOp): Boolean; virtual;
    procedure Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult); virtual;
    property VarType: TVarType read FVarType;
  end;
  TCustomVariantTypeClass = class of TCustomVariantType;

  TVarDataArray = array of TVarData;
  IVarInvokeable = interface
    ['{1CB65C52-BBCB-41A6-9E58-7FB916BEEB2D}']
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean;
  end;

  TInvokeableVariantType = class(TCustomVariantType, IVarInvokeable)
  protected
    procedure DispInvoke(var Dest: TVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
  public
    { IVarInvokeable }
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; virtual;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; virtual;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; virtual;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; virtual;
  end;

  IVarInstanceReference = interface
    ['{5C176802-3F89-428D-850E-9F54F50C2293}']
    function GetInstance(const V: TVarData): TObject;
  end;

  TPublishableVariantType = class(TInvokeableVariantType, IVarInstanceReference)
  protected
    { IVarInstanceReference }
    function GetInstance(const V: TVarData): TObject; virtual; abstract;
  public
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  function FindCustomVariantType(const AVarType: TVarType;
    out CustomVariantType: TCustomVariantType): Boolean; overload;
  function FindCustomVariantType(const TypeName: string;
    out CustomVariantType: TCustomVariantType): Boolean; overload;

type
  TAnyProc = procedure (var V: TVarData);
  TVarDispProc = procedure (Dest: PVariant; const Source: Variant;
      CallDesc: PCallDesc; Params: Pointer); cdecl;

Const
  CMaxNumberOfCustomVarTypes = $06FF;
  CMinVarType = $0100;
  CMaxVarType = CMinVarType + CMaxNumberOfCustomVarTypes;
  CIncVarType = $000F;
  CFirstUserType = CMinVarType + CIncVarType;

var
  NullEqualityRule: TNullCompareRule = ncrLoose;
  NullMagnitudeRule: TNullCompareRule = ncrLoose;
  NullStrictConvert: Boolean = true;

  VarDispProc: TVarDispProc;
  ClearAnyProc: TAnyProc;  { Handler clearing a varAny }
  ChangeAnyProc: TAnyProc; { Handler to change any to variant }
  RefAnyProc: TAnyProc;    { Handler to add a reference to an varAny }
  InvalidCustomVariantType : TCustomVariantType;

procedure VarCastError;
procedure VarCastError(const ASourceType, ADestType: TVarType);
procedure VarInvalidOp;
procedure VarInvalidNullOp;
procedure VarBadTypeError;
procedure VarOverflowError;
procedure VarOverflowError(const ASourceType, ADestType: TVarType);
procedure VarBadIndexError;
procedure VarArrayLockedError;
procedure VarNotImplError;
procedure VarOutOfMemoryError;
procedure VarInvalidArgError;
procedure VarInvalidArgError(AType: TVarType);
procedure VarUnexpectedError;
procedure VarRangeCheckError(const AType: TVarType);
procedure VarRangeCheckError(const ASourceType, ADestType: TVarType);
procedure VarArrayCreateError;
procedure VarResultCheck(AResult: HRESULT);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
procedure VarResultCheck(AResult: HRESULT; ASourceType, ADestType: TVarType);
procedure HandleConversionException(const ASourceType, ADestType: TVarType);
function VarTypeAsText(const AType: TVarType): string;
function FindVarData(const V: Variant): PVarData;

{ Typinfo unit variant routines have been moved here, so as not to make TypInfo dependent on variants }

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
Procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
Function  GetVariantProp(Instance: TObject; PropInfo : PPropInfo): Variant;
Function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
Procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
Procedure SetVariantProp(Instance: TObject; PropInfo : PPropInfo; const Value: Variant);

implementation

uses
  math,varutils;

  var
    customvarianttypes : array of tcustomvarianttype;
    customvarianttypelock : trtlcriticalsection;

procedure sysvarclearproc(var v : tvardata);forward;


function aligntoptr(p : pointer) : pointer;inline;
  begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    if (ptrint(p) mod sizeof(ptrint))<>0 then
      inc(ptrint(p),sizeof(ptrint)-ptrint(p) mod sizeof(ptrint));
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    result:=p;
  end;


{ ---------------------------------------------------------------------
    String Messages
  ---------------------------------------------------------------------}

ResourceString
  SErrVarIsEmpty = 'Variant is empty';
  SErrInvalidIntegerRange = 'Invalid Integer range: %d';

{ ---------------------------------------------------------------------
    Auxiliary routines
  ---------------------------------------------------------------------}

Procedure VariantError (Const Msg : String);
begin
  Raise EVariantError.Create(Msg);
end;


Procedure NotSupported(Meth: String);
begin
  Raise EVariantError.CreateFmt('Method %s not yet supported.',[Meth]);
end;

type
  tvariantarrayiter = object
    bounds : pvararrayboundarray;
    coords : pvararraycoorarray;
    dims : SizeInt;
    constructor init(d: SizeInt;b : pvararrayboundarray);
    function next : boolean;
    destructor done;
  end;


constructor tvariantarrayiter.init(d: SizeInt;b : pvararrayboundarray);
  var
    i : sizeint;
  begin
    bounds:=b;
    dims:=d;
    getmem(coords,sizeof(Sizeint)*dims);
    { initialize coordinate counter }
    for i:=0 to dims-1 do
      coords^[i]:=bounds^[i].lowbound;
  end;


function tvariantarrayiter.next : boolean;
  var
    finished : boolean;

  procedure incdim(d : SizeInt);
    begin
      if finished then
        exit;
      inc(coords^[d]);
      if coords^[d]>=bounds^[d].lowbound+bounds^[d].elementcount then
        begin
          coords^[d]:=bounds^[d].lowbound;
          if d>0 then
            incdim(d-1)
          else
            finished:=true;
        end;
    end;

  begin
    finished:=false;
    incdim(dims-1);
    result:=not(finished);
  end;


destructor tvariantarrayiter.done;
  begin
    freemem(coords);
  end;


type
  tdynarraybounds = array of SizeInt;
  tdynarraycoords = tdynarraybounds;
  tdynarrayelesize = tdynarraybounds;
  tdynarraypositions = array of pointer;
  tdynarrayiter = object
    bounds : tdynarraybounds;
    coords : tdynarraycoords;
    elesize : tdynarrayelesize;
    positions : tdynarraypositions;
    dims : SizeInt;
    data : pointer;
    constructor init(d : pointer;p : pdynarraytypeinfo;_dims: SizeInt;b : tdynarraybounds);
    function next : boolean;
    destructor done;
  end;


constructor tdynarrayiter.init(d : pointer;p : pdynarraytypeinfo;_dims: SizeInt;b : tdynarraybounds);
  var
    i : sizeint;
  begin
    bounds:=b;
    dims:=_dims;
    SetLength(coords,dims);
    SetLength(elesize,dims);
    SetLength(positions,dims);
    positions[0]:=d;
    { initialize coordinate counter and elesize }
    for i:=0 to dims-1 do
      begin
        coords[i]:=0;
        if i>0 then
          positions[i]:=pointer(positions[i-1]^);
        { skip kind and name }
        inc(pointer(p),ord(pdynarraytypeinfo(p)^.namelen)+2);

        p:=aligntoptr(p);

        elesize[i]:=psizeint(p)^;

        { skip elesize }
        inc(pointer(p),sizeof(sizeint));

        p:=pdynarraytypeinfo(ppointer(p)^);
      end;
    data:=positions[dims-1];
  end;


function tdynarrayiter.next : boolean;
  var
    finished : boolean;

  procedure incdim(d : SizeInt);
    begin
      if finished then
        exit;
      inc(coords[d]);
      inc(pointer(positions[d]),elesize[d]);

      if coords[d]>=bounds[d] then
        begin
          coords[d]:=0;
          if d>0 then
            begin
              incdim(d-1);
              positions[d]:=pointer(positions[d-1]^);
            end
          else
            finished:=true;
        end;
    end;

  begin
    finished:=false;
    incdim(dims-1);
    data:=positions[dims-1];
    result:=not(finished);
  end;


destructor tdynarrayiter.done;
  begin
    bounds:=nil;
    coords:=nil;
    elesize:=nil;
    positions:=nil;
  end;

{ ---------------------------------------------------------------------
    VariantManager support
  ---------------------------------------------------------------------}

procedure sysvarinit(var v : variant);
begin
  VariantInit(TVarData(V));
end;


procedure sysvarclear(var v : variant);
begin
  varclearproc(TVarData(V));
end;


function Sysvartoint (const v : variant) : longint;
begin
  Result:=VariantToLongint(TVarData(V));
end;


function Sysvartoint64 (const v : variant) : int64;
begin
  Result:=VariantToInt64(TVarData(V));
end;


function sysvartoword64 (const v : variant) : qword;
begin
  Result:=VariantToQWord (TVarData(V));
end;


function sysvartobool (const v : variant) : boolean;
begin
  Result:=VariantToBoolean(TVarData(V));
end;


function sysvartoreal (const v : variant) : extended;
begin
  Result:=VariantToDouble(TVarData(V));
end;


function sysvartocurr (const v : variant) : currency;
begin
  Result:=VariantToCurrency(TVarData(V));
end;


procedure sysvartolstr (var s : ansistring;const v : variant);
  begin
    S:=VariantToAnsiString(TVarData(V));
  end;


procedure sysvartopstr (var s;const v : variant);
  Var
    T : String;
  begin
    SysVarToLstr(T,V);
    ShortString(S):=T;
  end;


procedure sysvartowstr (var s : widestring;const v : variant);
  begin
    case tvardata(v).vtype of
      varString:
        s:=ansistring(tvardata(v).vstring);
      else
        s:=VariantToWideString(tvardata(v));
     end;
  end;


procedure sysvartointf (var intf : iinterface;const v : variant);
  begin
    case TVarData(v).vtype of
      varunknown:
        intf:=iinterface(TVarData(v).VUnknown);
      else
        begin
          varcasterror(TVarData(v).vtype,varunknown);
        end;
    end;
  end;


procedure sysvartodisp (var disp : idispatch;const v : variant);
begin
  NotSupported('VariantManager.sysvartodisp')
end;


function sysvartotdatetime (const v : variant) : tdatetime;
begin
  case TVarData(v).vtype of
    varString:
      if not(TryStrToDateTime(ansistring(tvardata(v).vstring),result)) and
        not(TryStrToDate(ansistring(tvardata(v).vstring),result)) then
        varcasterror(TVarData(v).vtype,vardate);
    varVariant:
      result:=sysvartotdatetime(PVariant(TVarData(V).VPointer)^);
    else
      result:=VariantToDate(TVarData(v));
  end;
end;


{$ifdef dummy}
function DynamicArrayDimensions(const p : pointer) : sizeint;
  begin
    result:=0;
    while assigned(pdynarraytypeinfo(p)) and (pdynarraytypeinfo(p)^.kind=byte(tkDynArray)) do
      begin
        inc(result);

        { skip kind and name }
        inc(pointer(p),ord(pdynarraytypeinfo(p)^.namelen)+2);

        p:=aligntoptr(p);

         p:=pdynarraytypeinfo(p+sizeof(sizeint))^;
      end;
  end;
{$endif dummy}


function DynamicArrayIsRectangular(p : pointer;typeinfo : pointer) : boolean;
  var
    arraysize,i : sizeint;
  begin
    result:=false;

    { get typeinfo of second level }
    { skip kind and name }
    inc(pointer(typeinfo),ord(pdynarraytypeinfo(typeinfo)^.namelen)+2);
    typeinfo:=aligntoptr(typeinfo);
    typeinfo:=ppointer(typeinfo+sizeof(sizeint))^;

    { check recursively? }
    if assigned(pdynarraytypeinfo(typeinfo)) and (pdynarraytypeinfo(typeinfo)^.kind=byte(tkDynArray)) then
      begin
        { set to dimension of first element }
        arraysize:=psizeint(ppointer(p)^-sizeof(sizeint))^;
        { walk through all elements }
        for i:=1 to psizeint(p-sizeof(sizeint))^ do
          begin
            { ... and check dimension }
            if psizeint(ppointer(p)^-sizeof(sizeint))^<>arraysize then
              exit;
            if not(DynamicArrayIsRectangular(ppointer(p)^,typeinfo)) then
              exit;
            inc(p,sizeof(pointer));
          end;
      end;
      result:=true;
    end;


procedure sysvartodynarray (var dynarr : pointer;const v : variant; typeinfo : pointer);
  begin
    DynArrayFromVariant(dynarr,v,typeinfo);
    if not(assigned(dynarr)) then
      VarCastError;
  end;


procedure sysvarfrombool (var dest : variant;const source : Boolean);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        VType:=varBoolean;
        VBoolean:=Source;
      end;
  end;


procedure sysvarfromint (var dest : variant;const source,range : longint);

begin
  if TVarData(Dest).VType>=varOleStr then
    sysvarclear(Dest);
  With TVarData(dest) do
    begin
    Case Range of
    -4 : begin
         vtype:=varinteger;
         vInteger:=Source;
         end;
    -2 : begin
         vtype:=varsmallInt;
         vSmallInt:=Source;
         end;
    -1 : Begin
         vtype:=varshortInt;
         vshortint:=Source;
         end;
     1 : begin
         vtype:=varByte;
         vByte:=Source;
         end;
     2 : begin
         vtype:=varWord;
         vWord:=Source;
         end;
     4 : Begin
         vtype:=varLongWord;
         vLongWord:=Source;
         end;
    else
       VariantError(Format(SErrInvalidIntegerRange,[Range]));
    end;
    end;
end;

procedure sysvarfromint64 (var dest : variant;const source : int64);

begin
  if TVarData(Dest).VType>=varOleStr then
    sysvarclear(Dest);
  With TVarData(dest) do
    begin
    vtype:=varint64;
    vInt64:=Source;
    end;
end;

procedure sysvarfromword64 (var dest : variant;const source : qword);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
      vtype:=varQWord;
      vQword:=Source;
      end;
  end;


procedure sysvarfromreal (var dest : variant;const source : extended);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        vtype:=varDouble;
        vDouble:=Source;
      end;
  end;


procedure sysvarfromsingle (var dest : variant;const source : single);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        vtype:=varSingle;
        vDouble:=Source;
      end;
  end;


procedure sysvarfromdouble (var dest : variant;const source : double);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        vtype:=varDouble;
        vDouble:=Source;
      end;
  end;


procedure sysvarfromcurr (var dest : variant;const source : currency);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        vtype:=varCurrency;
        vCurrency:=Source;
      end;
  end;


procedure sysvarfromtdatetime (var dest : variant;const source : tdatetime);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest);
    With TVarData(dest) do
      begin
        vtype:=varDate;
        vDate:=Source;
      end;
  end;


procedure sysvarfrompstr (var dest : variant;const source : shortstring);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest)
    else
      fillchar(dest,sizeof(dest),0);
    With TVarData(dest) do
      begin
        vtype:=varstring;
        vstring:=nil;
        ansistring(vString):=source;
      end;
  end;


procedure sysvarfromlstr (var dest : variant;const source : string);
  begin
    If TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest)
    else
      fillchar(dest,sizeof(dest),0);
    With TVarData(Dest) do
      begin
        vtype:=varstring;
        vstring:=nil;
        ansistring(vString):=source;
      end;
  end;


procedure sysvarfromwstr (var dest : variant;const source : widestring);
  begin
    If TVarData(Dest).VType>=varOleStr then
      sysvarclear(Dest)
    else
      fillchar(dest,sizeof(dest),0);
    With TVarData(Dest) do
      begin
        vtype:=varolestr;
        widestring(pointer(vOlestr)):=copy(source,1,MaxInt);
      end;
  end;


type
  tcommontype = (ct_empty,ct_any,ct_error,ct_longint,ct_float,ct_boolean,
    ct_int64,ct_nil,ct_widestr,ct_date,ct_currency,ct_string);

const
  { get the basic type for a variant type }
  vtypemap : array[varempty..varqword] of tcommontype =
    (ct_empty,           // varempty = 0;
     ct_nil,             // varnull = 1;
     ct_longint,         // varsmallint = 2;
     ct_longint,         // varinteger = 3;
     ct_float,           // varsingle = 4;
     ct_float,           // vardouble = 5;
     ct_currency,        // varcurrency = 6;
     ct_date,            // vardate = 7;
     ct_widestr,         // varolestr = 8;
     ct_error,           // vardispatch = 9;
     ct_error,           // varerror = 10;
     ct_boolean,         // varboolean = 11;
     ct_error,           // varvariant = 12;
     ct_error,           // varunknown = 13;
     ct_error,           // ??? 15
     ct_error,           // vardecimal = 14;
     ct_longint,         // varshortint = 16;
     ct_longint,         // varbyte = 17;
     ct_longint,         // varword = 18;
     ct_int64,           // varlongword = 19;
     ct_int64,           // varint64 = 20;
     ct_int64            // varqword = 21;
    );

  { map a basic type back to a variant type }
  commontypemap : array[tcommontype] of word =
    (
      varempty,
      varany,
      varerror,
      varinteger,
      vardouble,
      varboolean,
      varint64,
      varnull,
      varolestr,
      vardate,
      varcurrency,
      varstring
    );

function maptocommontype(const vtype : tvartype) : tcommontype;
  begin
    case vtype and vartypemask of
      varString:
        result:=ct_string;
      varAny:
        result:=ct_any;
      else
        begin
          if ((vtype and vartypemask)>=low(vtypemap)) and ((vtype and vartypemask)<=high(vtypemap)) then
            result:=vtypemap[vtype and vartypemask]
          else
            result:=ct_error;
        end;
    end;
  end;

const
  findcmpcommontype : array[tcommontype,tcommontype] of tcommontype = (
     {               ct_emtpy    ct_any   ct_error ct_longint  ct_float    ct_boolean  ct_int64    ct_nil   ct_widestr  ct_date  ct_currency ct_string  }
    ({ ct_empty }    ct_empty,   ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_string  ),
    ({ ct_any }      ct_any,     ct_any,  ct_error,ct_any,     ct_any,     ct_any,     ct_any,     ct_any,  ct_any,     ct_any,  ct_any,     ct_any     ),
    ({ ct_error }    ct_error,   ct_error,ct_error,ct_error,   ct_error,   ct_error,   ct_error,   ct_error,ct_error,   ct_error,ct_error,   ct_error   ),
    ({ ct_longint }  ct_longint, ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_float }    ct_float,   ct_any,  ct_error,ct_float,   ct_float,   ct_float,   ct_float,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_boolean }  ct_boolean, ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_string  ),
    ({ ct_int64 }    ct_int64,   ct_any,  ct_error,ct_int64,   ct_float,   ct_int64,   ct_int64,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_nil }      ct_nil,     ct_any,  ct_error,ct_nil,     ct_nil,     ct_nil,     ct_nil,     ct_nil,  ct_nil,     ct_nil,  ct_nil,     ct_nil     ),
    ({ ct_widestr }  ct_widestr, ct_any,  ct_error,ct_float,   ct_float,   ct_widestr, ct_float,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_widestr ),
    ({ ct_date }     ct_date,    ct_any,  ct_error,ct_date,    ct_date,    ct_date,    ct_date,    ct_nil,  ct_date,    ct_date, ct_date,    ct_date    ),
    ({ ct_currency } ct_currency,ct_any,  ct_error,ct_currency,ct_currency,ct_currency,ct_currency,ct_nil,  ct_currency,ct_date, ct_currency,ct_currency),
    ({ ct_string }   ct_string,  ct_any,  ct_error,ct_float,   ct_float,   ct_string,  ct_float,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_string)
    );

function dovarcmpempty(const vl,vr : tvardata) : shortint;
  begin
    if vl.vtype=varempty then
      begin
        if vr.vtype=varempty then
          result:=0
        else
          result:=-1;
      end
    else if vr.vtype=varempty then
      result:=1;
  end;


function dovarcmpnull(const vl,vr : tvardata) : shortint;
  begin
    if vl.vtype=varnull then
      begin
        if vr.vtype=varnull then
          result:=0
        else
          result:=-1;
      end
    else if vr.vtype=varnull then
      result:=1;
  end;


function dovarcmp (const vl,vr : tvardata;const opcode : tvarop) : shortint;
  var
    resulttype : longint;

    { use a variant here for proper init./finalization }
    vlconv,vrconv : variant;

    variantmanager : tvariantmanager;
  begin
    result:=0;
    { variant reference? }
    if vl.vtype=(varbyref or varvariant) then
      result:=dovarcmp(tvardata(vl.vpointer^),vr,opcode)
    else if vr.vtype=(varbyref or varvariant) then
      result:=dovarcmp(vl,tvardata(vr.vpointer^),opcode)
    { one is empty? }
    else if vr.vtype=varempty then
      result:=dovarcmpempty(vl,vr)
    else if vl.vtype=varempty then
      result:=dovarcmpempty(vl,vr)
    { one is null? }
    else if vr.vtype=varnull then
      result:=dovarcmpnull(vl,vr)
    else if vl.vtype=varnull then
      result:=dovarcmpnull(vl,vr)
    else
      begin
        GetVariantManager(variantmanager);
        { cast both to a common type }
        resulttype:=commontypemap[findcmpcommontype[maptocommontype(vl.vtype),maptocommontype(vr.vtype)]];
        variantmanager.varcast(vlconv,variant(vl),resulttype);
        variantmanager.varcast(vrconv,variant(vr),resulttype);

        { sanity check }
        if tvardata(vlconv).vtype<>tvardata(vrconv).vtype then
          VarInvalidOp;

        case tvardata(vlconv).vtype of
          varempty:
            begin
              if tvardata(vlconv).vtype=varempty then
                if tvardata(vrconv).vtype=varempty then
                  result:=0
                else
                  result:=-1
              else
                result:=1;
            end;
          //!!!! varany:

          varerror:
            VarInvalidOp;

          varinteger:
            begin
              if tvardata(vlconv).vinteger>tvardata(vrconv).vinteger then
                result:=1
              else if tvardata(vlconv).vinteger<tvardata(vrconv).vinteger then
                result:=-1
              else
                result:=0;
            end;

          vardouble:
            begin
              if tvardata(vlconv).vdouble>tvardata(vrconv).vdouble then
                result:=1
              else if tvardata(vlconv).vdouble<tvardata(vrconv).vdouble then
                result:=-1
              else
                result:=0;
            end;

          varboolean:
            begin
              if ord(tvardata(vlconv).vboolean)>ord(tvardata(vrconv).vboolean) then
                result:=1
              else if ord(tvardata(vlconv).vboolean)<ord(tvardata(vrconv).vboolean) then
                result:=-1
              else
                result:=0;
            end;

          varint64:
            begin
              if tvardata(vlconv).vint64>tvardata(vrconv).vint64 then
                result:=1
              else if tvardata(vlconv).vint64<tvardata(vrconv).vint64 then
                result:=-1
              else
                result:=0;
            end;

          varnull:
            begin
              case NullEqualityRule of
                ncrError:
                  VarInvalidOp;
                ncrStrict:
                  { force false }
                  case opcode of
                    opcmpeq:
                      result:=-1;
                    opcmpne:
                      result:=0;
                    opcmplt:
                      result:=1;
                    opcmple:
                      result:=1;
                    opcmpgt:
                      result:=-1;
                    opcmpge:
                      result:=-1;
                    else
                      VarInvalidOp;
                  end;
                ncrLoose:
                  begin
                    if tvardata(vlconv).vtype=varnull then
                      if tvardata(vrconv).vtype=varnull then
                        result:=0
                      else
                        result:=-1
                    else
                      result:=1;
                  end;
                else
                  VarInvalidOp;
              end;
            end;


          varolestr:
            result:=WideCompareStr(ansistring(tvardata(vlconv).volestr),ansistring(tvardata(vrconv).volestr));

          vardate:
            begin
              if tvardata(vlconv).vdate>tvardata(vrconv).vdate then
                result:=1
              else if tvardata(vlconv).vdate<tvardata(vrconv).vdate then
                result:=-1
              else
                result:=0;
            end;

          varcurrency:
            begin
              if tvardata(vlconv).vcurrency>tvardata(vrconv).vcurrency then
                result:=1
              else if tvardata(vlconv).vcurrency<tvardata(vrconv).vcurrency then
                result:=-1
              else
                result:=0;
            end;

          varstring:
            result:=AnsiCompareStr(ansistring(tvardata(vlconv).vstring),ansistring(tvardata(vrconv).vstring));
        else
          VarInvalidOp;
        end;
      end;
  end;


function syscmpop (const left,right : variant;const opcode : tvarop) : boolean;
  var
    cmpres : shortint;
  begin
    cmpres:=dovarcmp(tvardata(left),tvardata(right),opcode);
    case opcode of
      opcmpeq:
        result:=cmpres=0;
      opcmpne:
        result:=cmpres<>0;
      opcmplt:
        result:=cmpres<0;
      opcmple:
        result:=cmpres<=0;
      opcmpgt:
        result:=cmpres>0;
      opcmpge:
        result:=cmpres>=0;
     else
       VarInvalidOp;
    end;
  end;


const
  findopcommontype : array[tcommontype,tcommontype] of tcommontype = (
     {               ct_emtpy    ct_any   ct_error ct_longint  ct_float    ct_boolean  ct_int64    ct_nil   ct_widestr  ct_date  ct_currency ct_string  }
    ({ ct_empty }    ct_empty,   ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_string  ),
    ({ ct_any }      ct_any,     ct_any,  ct_error,ct_any,     ct_any,     ct_any,     ct_any,     ct_any,  ct_any,     ct_any,  ct_any,     ct_any     ),
    ({ ct_error }    ct_error,   ct_error,ct_error,ct_error,   ct_error,   ct_error,   ct_error,   ct_error,ct_error,   ct_error,ct_error,   ct_error   ),
    ({ ct_longint }  ct_longint, ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_float }    ct_float,   ct_any,  ct_error,ct_float,   ct_float,   ct_float,   ct_float,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_boolean }  ct_boolean, ct_any,  ct_error,ct_longint, ct_float,   ct_boolean, ct_int64,   ct_nil,  ct_boolean, ct_date, ct_currency,ct_boolean ),
    ({ ct_int64 }    ct_int64,   ct_any,  ct_error,ct_int64,   ct_float,   ct_int64,   ct_int64,   ct_nil,  ct_float,   ct_date, ct_currency,ct_float   ),
    ({ ct_nil }      ct_nil,     ct_any,  ct_error,ct_nil,     ct_nil,     ct_nil,     ct_nil,     ct_nil,  ct_nil,     ct_nil,  ct_nil,     ct_nil     ),
    ({ ct_widestr }  ct_widestr, ct_any,  ct_error,ct_float,   ct_float,   ct_boolean, ct_float,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_widestr ),
    ({ ct_date }     ct_date,    ct_any,  ct_error,ct_date,    ct_date,    ct_date,    ct_date,    ct_nil,  ct_date,    ct_date, ct_date,    ct_date    ),
    ({ ct_currency } ct_currency,ct_any,  ct_error,ct_currency,ct_currency,ct_currency,ct_currency,ct_nil,  ct_currency,ct_date, ct_currency,ct_currency),
    ({ ct_string }   ct_string,  ct_any,  ct_error,ct_float,   ct_float,   ct_boolean, ct_float,   ct_nil,  ct_widestr, ct_date, ct_currency,ct_string)
    );


function dovarop(const vl,vr : tvardata;const opcode : tvarop) : tvardata;
  var
    resulttype : longint;

    { use a variant here for proper init./finalization }
    vlconv,vrconv : variant;
    tryint64,tryreal : boolean;

    variantmanager : tvariantmanager;
  begin
    fillchar(result,sizeof(result),0);
    { variant reference? }
    if vl.vtype=(varbyref or varvariant) then
      result:=dovarop(tvardata(vl.vpointer^),vr,opcode)
    else if vr.vtype=(varbyref or varvariant) then
      result:=dovarop(vl,tvardata(vr.vpointer^),opcode)
    { one is empty? }
    else if (vr.vtype = varempty) or (vl.vtype = varempty) then
     result.vtype:=varempty
    { one is null? }
    else if (vr.vtype = varnull) or (vl.vtype = varnull) then
      result.vtype:=varnull
    else
      begin
        GetVariantManager(variantmanager);
        { cast both to a common type }
        resulttype:=commontypemap[findopcommontype[maptocommontype(vl.vtype),maptocommontype(vr.vtype)]];
        variantmanager.varcast(vlconv,variant(vl),resulttype);
        variantmanager.varcast(vrconv,variant(vr),resulttype);

        { sanity check }
        if tvardata(vlconv).vtype<>tvardata(vrconv).vtype then
          VarInvalidOp;

        case tvardata(vlconv).vtype of
{
          varempty:
            // both must be empty then
            result:=0;
          //!!!! varany:

          varerror:
            VarInvalidOp;
}
          varinteger:
            begin
              tryint64:=false;
              result.vtype:=varinteger;
{$r+,q+}
              try

                case opcode of
                  opadd:
                    result.vinteger:=tvardata(vlconv).vinteger+tvardata(vrconv).vinteger;
                  opsubtract:
                    result.vinteger:=tvardata(vlconv).vinteger-tvardata(vrconv).vinteger;
                  opmultiply:
                    result.vinteger:=tvardata(vlconv).vinteger*tvardata(vrconv).vinteger;
                  opintdivide:
                    result.vinteger:=tvardata(vlconv).vinteger div tvardata(vrconv).vinteger;
                  oppower:
                    result.vinteger:=tvardata(vlconv).vinteger**tvardata(vrconv).vinteger;
                  opmodulus:
                    result.vinteger:=tvardata(vlconv).vinteger mod tvardata(vrconv).vinteger;
                  opshiftleft:
                    result.vinteger:=tvardata(vlconv).vinteger shl tvardata(vrconv).vinteger;
                  opshiftright:
                    result.vinteger:=tvardata(vlconv).vinteger shr tvardata(vrconv).vinteger;
                  opand:
                    result.vinteger:=tvardata(vlconv).vinteger and tvardata(vrconv).vinteger;
                  opor:
                    result.vinteger:=tvardata(vlconv).vinteger or tvardata(vrconv).vinteger;
                  opxor:
                    result.vinteger:=tvardata(vlconv).vinteger xor tvardata(vrconv).vinteger;
                  opdivide:
                    begin
                      result.vtype:=vardouble;
                      result.vdouble:=tvardata(vlconv).vinteger/tvardata(vrconv).vinteger;
                    end;
                  else
                    VarInvalidOp;
                end;
              except
                on erangeerror do
                  tryint64:=true;
                on eoverflow do
                  tryint64:=true;
              end;
{$r-,q-}
              if tryint64 then
                begin
                  variantmanager.varcast(vlconv,vlconv,varint64);
                  variantmanager.varcast(vrconv,vrconv,varint64);
                  variantmanager.varop(vlconv,vrconv,opcode);
                end;
            end;

          vardouble:
            begin
              result.vtype:=vardouble;
              case opcode of
                opadd:
                  result.vdouble:=tvardata(vlconv).vdouble+tvardata(vrconv).vdouble;
                opsubtract:
                  result.vdouble:=tvardata(vlconv).vdouble-tvardata(vrconv).vdouble;
                opmultiply:
                  result.vdouble:=tvardata(vlconv).vdouble*tvardata(vrconv).vdouble;
                oppower:
                  result.vdouble:=tvardata(vlconv).vdouble**tvardata(vrconv).vdouble;
                opdivide:
                  result.vdouble:=tvardata(vlconv).vdouble/tvardata(vrconv).vdouble;
                else
                  VarInvalidOp;
              end;
            end;

          varboolean:
            begin
              result.vtype:=varboolean;
              case opcode of
                opadd,opsubtract,opmultiply,opintdivide,oppower,
                opmodulus,opshiftleft,opshiftright:
                  begin
                    variantmanager.varcast(vlconv,vlconv,varinteger);
                    variantmanager.varcast(vrconv,vrconv,varinteger);
                    variantmanager.varop(vlconv,vrconv,opcode);
                  end;
                opand:
                  result.vboolean:=tvardata(vlconv).vboolean and tvardata(vrconv).vboolean;
                opor:
                  result.vboolean:=tvardata(vlconv).vboolean or tvardata(vrconv).vboolean;
                opxor:
                  result.vboolean:=tvardata(vlconv).vboolean xor tvardata(vrconv).vboolean;
                opdivide:
                  begin
                    variantmanager.varcast(vlconv,vlconv,vardouble);
                    variantmanager.varcast(vrconv,vrconv,vardouble);
                    variantmanager.varop(vlconv,vrconv,opcode);
                  end;
                else
                  VarInvalidOp;
              end;
            end;

          varint64:
            begin
              tryreal:=false;
              result.vtype:=varint64;
{$r+,q+}
              try

                case opcode of
                  opadd:
                    result.vint64:=tvardata(vlconv).vint64+tvardata(vrconv).vint64;
                  opsubtract:
                    result.vint64:=tvardata(vlconv).vint64-tvardata(vrconv).vint64;
                  opmultiply:
                    result.vint64:=tvardata(vlconv).vint64*tvardata(vrconv).vint64;
                  opintdivide:
                    result.vint64:=tvardata(vlconv).vint64 div tvardata(vrconv).vint64;
                  oppower:
                    result.vint64:=tvardata(vlconv).vint64**tvardata(vrconv).vint64;
                  opmodulus:
                    result.vint64:=tvardata(vlconv).vint64 mod tvardata(vrconv).vint64;
                  opshiftleft:
                    result.vint64:=tvardata(vlconv).vint64 shl tvardata(vrconv).vint64;
                  opshiftright:
                    result.vint64:=tvardata(vlconv).vint64 shr tvardata(vrconv).vint64;
                  opand:
                    result.vint64:=tvardata(vlconv).vint64 and tvardata(vrconv).vint64;
                  opor:
                    result.vint64:=tvardata(vlconv).vint64 or tvardata(vrconv).vint64;
                  opxor:
                    result.vint64:=tvardata(vlconv).vint64 xor tvardata(vrconv).vint64;
                  opdivide:
                    begin
                      result.vtype:=vardouble;
                      result.vdouble:=tvardata(vlconv).vint64/tvardata(vrconv).vint64;
                    end;
                  else
                    VarInvalidOp;
                end;
              except
                on erangeerror do
                  tryreal:=true;
                on eoverflow do
                  tryreal:=true;
              end;
{$r-,q-}
              if tryreal then
                begin
                  variantmanager.varcast(vlconv,vlconv,vardouble);
                  variantmanager.varcast(vrconv,vrconv,vardouble);
                  variantmanager.varop(vlconv,vrconv,opcode);
                end;
            end;
{
          //!!!! varnull:
          varolestr:
            result:=WideCompareStr(ansistring(tvardata(vlconv).volestr),ansistring(tvardata(vrconv).volestr));

          vardate:
            begin
            end;

          varcurrency:
            begin
            end;
}
          varstring:
            begin
              result.vtype:=varstring;
              case opcode of
                opadd:
                  ansistring(result.vstring):=ansistring(tvardata(vlconv).vstring)+ansistring(tvardata(vrconv).vstring);
                opdivide,
                opsubtract,
                opmultiply,
                oppower:
                  begin
                    variantmanager.varcast(vlconv,vlconv,vardouble);
                    variantmanager.varcast(vrconv,vrconv,vardouble);
                    variantmanager.varop(vlconv,vrconv,opcode);
                  end;

                opintdivide,
                opmodulus,
                opshiftleft,
                opshiftright,
                opand,
                opor,
                opxor:
                  begin
                    variantmanager.varcast(vlconv,vlconv,varinteger);
                    variantmanager.varcast(vrconv,vrconv,varinteger);
                    variantmanager.varop(vlconv,vrconv,opcode);
                  end;
                else
                  VarInvalidOp;
              end;
            end;
        else
          VarInvalidOp;
        end;
      end;
  end;


procedure sysvarop (var left : variant;const right : variant;opcode : tvarop);
  begin
    left:=variant(dovarop(tvardata(left),tvardata(right),opcode));
  end;


procedure sysvarneg (var v : variant);
  var
    customvarianttype : tcustomvarianttype;
  begin
    with tvardata(v) do
      case vtype of
       varempty:
         v:=smallint(0);
       varnull:
         ;
       varsmallint:
         vsmallint:=-vsmallint;
       varinteger:
         vinteger:=vinteger;
       varsingle:
         vsingle:=-vsingle;
       vardouble:
         vdouble:=-vdouble;
       varcurrency:
         vcurrency:=-vcurrency;
       vardate:
         vdate:=-vdate;
       varolestr:
         NotSupported('VariantManager.sysvarneg');
       vardispatch:
         NotSupported('VariantManager.sysvarneg');
       varerror:
         NotSupported('VariantManager.sysvarneg');
       varboolean:
         NotSupported('VariantManager.sysvarneg');
       varvariant:
         v:=-variant((tvardata(v).vpointer)^);
       varunknown:
         NotSupported('VariantManager.sysvarneg');
       vardecimal:
         NotSupported('VariantManager.sysvarneg');
       varshortint:
         vshortint:=-vshortint;
       varbyte:
         vbyte:=-vbyte;
       varword:
         vword:=-vword;
       varlongword:
         vlongword:=-vlongword;
       varint64:
         vint64:=-vint64;
       varqword:
         vqword:=-vqword;
       else
         begin
           if FindCustomVariantType(vtype,customvarianttype) then
             customvarianttype.UnaryOp(tvardata(v),opNegate)
           else
             VarInvalidOp;
         end;
  end;
end;


procedure sysvarnot (var v : variant);
  var
    customvarianttype : tcustomvarianttype;
  begin
    with tvardata(v) do
      case vtype of
       varempty:
         v:=smallint(-1);
       varnull:
         ;
       varsmallint:
         vsmallint:=not(vsmallint);
       varinteger:
         vinteger:=not(vinteger);
        {
       varsingle:
         vsingle:=-vsingle;
       vardouble:
         vdouble:=-vdouble;
       varcurrency:
         vcurrency:=-vcurrency;
       vardate:
         vdate:=-vdate;
        }
       varolestr:
         NotSupported('VariantManager.sysvarneg');
       vardispatch:
         NotSupported('VariantManager.sysvarneg');
       varerror:
         NotSupported('VariantManager.sysvarneg');
       varboolean:
         vboolean:=not(vboolean);
       varvariant:
         v:=not(variant((tvardata(v).vpointer)^));
       varunknown:
         NotSupported('VariantManager.sysvarneg');
       vardecimal:
         NotSupported('VariantManager.sysvarneg');
       varshortint:
         vshortint:=not(vshortint);
       varbyte:
         vbyte:=not(vbyte);
       varword:
         vword:=not(vword);
       varlongword:
         vlongword:=not(vlongword);
       varint64:
         vint64:=not(vint64);
       varqword:
         vqword:=not(vqword);
       else
         begin
           if FindCustomVariantType(vtype,customvarianttype) then
             customvarianttype.UnaryOp(tvardata(v),opNot)
           else
             VarInvalidOp;
         end;
    end;
  end;


procedure sysvarclearproc(var v : tvardata);
  var
    customvarianttype : tcustomvarianttype;
  begin
    { easy type? }
    if (v.vtype<varOleStr) or
      (v.vtype=varInt64) or (v.vtype=varQWord) then
      v.vtype:=varempty
    { type handled by varutils? }
    else if v.vtype<varInt64 then
      begin
      varresultcheck(variantclear(v));
      if ((V.vtype=varDispatch) or (V.vtype=varUnknown)) then
        v.vunknown:=Nil;
      end
    { pascal string? }
    else if v.vtype=varString then
      begin
        v.vtype:=varempty;
        ansistring(v.vstring):='';
      end
    { array? }
    else if (v.vtype and varArray)<>0 then
      begin
        varresultcheck(variantclear(v));
      end
    { corba? }
    else if v.vtype=varany then
      ClearAnyProc(v)
    { custom? }
    else if findcustomvarianttype(v.vtype,customvarianttype) then
      customvarianttype.clear(v)
    { varutils fallback }
    else
      varresultcheck(variantclear(v));
  end;


procedure sysvarcopyproc(var d : tvardata;const s : tvardata);
  var
    customvarianttype : tcustomvarianttype;
    p,newarray : pvararray;
    boundsarray : pvararrayboundarray;
    ubound : sizeint;
    iter : tvariantarrayiter;
    varfrom,varto : pvardata;
    i : SizeInt;
  begin
    if @d=@s then
      exit;
    sysvarclearproc(d);
    { easy type? }
    if (s.vtype<varOleStr) or
      (s.vtype=varInt64) or (s.vtype=varQWord) then
      d:=s
    { type handled by varutils? }
    else if s.vtype<varInt64 then
      varresultcheck(variantcopy(d,s))
    { pascal string? }
    else if s.vtype=varString then
      begin
        d.vtype:=varstring;
        d.vstring:=nil;
        ansistring(d.vstring):=ansistring(s.vstring);
      end
    { array? }
    else if (s.vtype and varArray)<>0 then
      begin
        { vararray of variant needs some extra work ... }
        if (s.vtype and varTypeMask)=varVariant then
          begin
            { get pointer to the array data }
            if (s.vtype and varByRef)<>0 then
              p:=pvararray(s.vpointer^)
            else
              p:=s.varray;

            getmem(boundsarray,p^.DimCount*sizeof(TVarArrayBound));
            try
              for i:=0 to p^.DimCount-1 do
                begin
                  VarResultCheck(SafeArrayGetLBound(p,i+1,boundsarray^[i].lowbound));
                  VarResultCheck(SafeArrayGetUBound(p,i+1,ubound));
                  boundsarray^[i].elementcount:=ubound-boundsarray^[i].lowbound+1;
                end;

              newarray:=SafeArrayCreate(varVariant,p^.DimCount,boundsarray^);
              if not(assigned(newarray)) then
                VarArrayCreateError;

              try
                iter.init(p^.DimCount,boundsarray);
                repeat
                  VarResultCheck(SafeArrayPtrOfIndex(p,iter.coords,varfrom));
                  VarResultCheck(SafeArrayPtrOfIndex(newarray,iter.coords,varto));
                  sysvarcopyproc(varto^,varfrom^);
                until not(iter.next);
                d.vtype:=varVariant or varArray;
                d.varray:=newarray;
              finally
                iter.done;
              end;
            finally
              freemem(boundsarray);
            end;
          end
        else
          varresultcheck(variantcopy(d,s));
      end
    { corba? }
    else if s.vtype=varany then
      NotSupported('VariantManager.sysvarcopyproc.varAny')
    { custom? }
    else if findcustomvarianttype(s.vtype,customvarianttype) then
      customvarianttype.copy(d,s,false)
    { varutils fallback }
    else
      varresultcheck(variantcopy(d,s));
  end;


procedure sysvaraddrefproc(var v : tvardata);
  var
    dummy : tvardata;
  begin
    { create a copy to a dummy }
    fillchar(dummy,sizeof(dummy),0);
    sysvarcopyproc(dummy,v);
  end;


procedure sysvaraddref(var v : variant);
  begin
    sysvaraddrefproc(tvardata(v));
  end;


procedure sysvarcopy (var dest : variant;const source : variant);
  begin
    sysvarcopyproc(tvardata(dest),tvardata(source));
  end;

function sysvarcastint64(const v : tvardata) : int64;
  begin
    try
      case v.vtype of
        varByte:
          result:=v.vbyte;
        varShortint:
          result:=v.vshortint;
        varSmallint:
          result:=v.vsmallint;
        varWord:
          result:=v.vword;
        varInteger:
          result:=v.vinteger;
        varLongword:
          result:=v.vlongword;
        varInt64:
          result:=v.vint64;
{$R+}
        varQWord:
          result:=v.vqword;
{$R-}
        else
          VarInvalidOp;
      end;
    except
      HandleConversionException(v.vtype,varint64);
      result:=0;
    end;
  end;

function sysvarcastword64(const v : tvardata) : qword;
  begin
    try
      case v.vtype of
{$R+}
        varShortint:
          result:=v.vshortint;
        varSmallint:
          result:=v.vsmallint;
        varInteger:
          result:=v.vinteger;
        varInt64:
          result:=v.vint64;
{$R-}
        varByte:
          result:=v.vbyte;
        varWord:
          result:=v.vword;
        varLongword:
          result:=v.vlongword;
        varQWord:
          result:=v.vqword;
        else
          VarInvalidOp;
      end;
    except
      HandleConversionException(v.vtype,varword64);
      result:=0;
    end;
  end;

function sysvarcastinteger(const v : tvardata) : longint;
  begin
    try
      case v.vtype of
        varByte:
          result:=v.vbyte;
        varShortint:
          result:=v.vshortint;
        varSmallint:
          result:=v.vsmallint;
        varWord:
          result:=v.vword;
        varInteger:
          result:=v.vinteger;
{$R+}
        varLongword:
          result:=v.vlongword;
        varQWord:
          result:=v.vqword;
        varInt64:
          result:=v.vint64;
{$R-}
        else
          VarInvalidOp;
      end;
    except
      HandleConversionException(v.vtype,varinteger);
      result:=0;
    end;
  end;


function sysvarcastreal(const v : tvardata) : double;
  begin
    try
      case v.vtype of
        varByte:
          result:=v.vbyte;
        varShortint:
          result:=v.vshortint;
        varSmallint:
          result:=v.vsmallint;
        varWord:
          result:=v.vword;
        varInteger:
          result:=v.vinteger;
        varLongword:
          result:=v.vlongword;
        varQWord:
          result:=v.vqword;
        varInt64:
          result:=v.vint64;
        varSingle:
          result:=v.vsingle;
        varDouble:
          result:=v.vdouble;
        varCurrency:
          result:=v.vcurrency;
        VarString  :
          begin
            if not(TryStrToFloat(ansistring(v.vString),Result)) then
              HandleConversionException(v.vtype,vardouble);
          end;
        else
          VariantToDouble(v);
      end;
    except
      HandleConversionException(v.vtype,vardouble);
      result:=0;
    end;
  end;


procedure sysvarcast (var dest : variant;const source : variant;vartype : longint);
  var
    customvarianttype : tcustomvarianttype;
    variantmanager : tvariantmanager;
  begin
    { already the type we want? }
    if tvardata(source).vtype=vartype then
      dest:=source
    else if tvardata(source).vtype=(varByRef or varVariant) then
      sysvarcast(dest,pvariant(tvardata(source).vpointer)^,vartype)
    else
      begin
        getVariantManager(variantmanager);
        case vartype of
          varAny:
            VarCastError(tvardata(source).vtype,vartype);
          varEmpty:
            if (tvardata(source).vtype=varNull) and NullStrictConvert then
              VarCastError(varNull,varEmpty)
            else
              SysVarClear(dest);
          varNull:
            begin
              SysVarClear(dest);
              tvardata(dest).vtype:=varNull;
            end;
          varint64:
            variantmanager.varfromint64(dest,sysvarcastint64(tvardata(source)));
          varword64:
            variantmanager.varfromword64(dest,sysvarcastword64(tvardata(source)));
          varinteger:
            variantmanager.varfromint(dest,sysvarcastinteger(tvardata(source)),-4);
          varsingle:
            { calling through the variantmanager isn't possible here because
              it doesn't provide different casts for singles und doubles (FK) }
            sysvarfromsingle(dest,sysvarcastreal(tvardata(source)));
          vardouble:
            sysvarfromdouble(dest,sysvarcastreal(tvardata(source)));
          else
            begin
              if findcustomvarianttype(tvardata(source).vtype,customvarianttype) then
                customvarianttype.CastTo(tvardata(dest),tvardata(source),vartype)
              else if FindCustomVariantType(vartype,customvarianttype) then
                customvarianttype.Cast(tvardata(dest),tvardata(source))
              else
                VarCastError(tvardata(source).vtype,vartype);
            end;
         end;
      end;
  end;


procedure sysvarfromintf(var dest : variant;const source : iinterface);
  begin
    sysvarclearproc(TVarData(dest));
    TVarData(dest).VUnknown:=nil;
    iinterface(TVarData(dest).VUnknown) := source;
    TVarData(dest).VType := varUnknown;
  end;


procedure sysvarfromdisp(var dest : variant;const source : idispatch);
  begin
  end;


procedure sysvarfromdynarray(var dest : variant;const source : pointer; typeinfo: pointer);
  begin
    DynArrayToVariant(dest,source,typeinfo);
    if VarIsEmpty(dest) then
      VarCastError;
  end;


procedure sysolevarfrompstr(var dest : olevariant; const source : shortstring);
  begin
    sysvarfromwstr(variant(tvardata(dest)),source);
  end;


procedure sysolevarfromlstr(var dest : olevariant; const source : ansistring);
  begin
    sysvarfromwstr(variant(tvardata(dest)),source);
  end;


procedure sysolevarfromvar(var dest : olevariant; const source : variant);
  begin
    if tvardata(source).vtype=varVariant or varByRef then
      sysolevarfromvar(dest,source)
    else
      begin
        case tvardata(source).vtype of
          varWord,varShortInt,varByte:
            sysvarcast(variant(tvardata(dest)),source,varInteger);
          varString:
            sysolevarfromlstr(dest,source);
        else
          VarCastError;
        end;
      end;
  end;


procedure sysolevarfromint(var dest : olevariant; const source : longint;const range : shortint);
  begin
    if TVarData(Dest).VType>=varOleStr then
      sysvarclear(variant(tvardata(Dest)));
    tvardata(dest).vtype:=varInteger;
    tvardata(dest).vinteger:=source;
  end;


procedure sysvarcastole(var dest : variant;const source : variant;vartype : longint);
  begin
    NotSupported('VariantManager.sysvarcastole');
  end;


procedure sysdispinvoke(dest : pvardata;const source : tvardata;calldesc : pcalldesc;params : pointer);cdecl;
  begin
    NotSupported('VariantManager.sysdispinvoke');
  end;


procedure sysvararrayredim(var a : variant;highbound : SizeInt);
  var
    src : tvardata;
    p : pvararray;
    newbounds : tvararraybound;
  begin
    src:=tvardata(a);
    { get final variant }
    while src.vtype=varByRef or varVariant do
      src:=tvardata(src.vpointer^);

    if (src.vtype and varArray)<>0 then
      begin
        { get pointer to the array }
        if (src.vtype and varByRef)<>0 then
          p:=pvararray(src.vpointer^)
        else
          p:=src.varray;

        if highbound<p^.bounds[p^.dimcount-1].lowbound then
          VarInvalidArgError;

        newbounds.lowbound:=p^.bounds[p^.dimcount-1].lowbound;
        newbounds.elementcount:=highbound-newbounds.lowbound+1;

        VarResultCheck(SafeArrayRedim(p,newbounds));
      end
    else
      VarInvalidArgError(src.vtype);
  end;


function getfinalvartype(v : tvardata) : tvartype;{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
  begin
    while v.vtype=varByRef or varVariant do
      v:=tvardata(v.vpointer^);
    result:=v.vtype;
  end;


function sysvararrayget(const a : variant;indexcount : SizeInt;indices : psizeint) : variant;cdecl;
  var
    src : tvardata;
    p : pvararray;
    arraysrc : pvariant;
    arrayelementtype : tvartype;
    data : pointer;
    variantmanager : tvariantmanager;
  begin
    src:=tvardata(a);
    { get final variant }
    while src.vtype=varByRef or varVariant do
      src:=tvardata(src.vpointer^);

    if (src.vtype and varArray)<>0 then
      begin
        { get pointer to the array }
        if (src.vtype and varByRef)<>0 then
          p:=pvararray(src.vpointer^)
        else
          p:=src.varray;

        { number of indices ok? }
        if p^.DimCount<>indexcount then
          VarInvalidArgError;

        arrayelementtype:=src.vtype and vartypemask;
        if arrayelementtype=varVariant then
          begin
            VarResultCheck(SafeArrayPtrOfIndex(p,pvararraycoorarray(indices),arraysrc));
            result:=arraysrc^;
          end
        else
          begin
            tvardata(result).vtype:=arrayelementtype;
            VarResultCheck(SafeArrayGetElement(p,pvararraycoorarray(indices),@tvardata(result).vpointer));
          end;
      end
    else
      VarInvalidArgError(src.vtype);
  end;


procedure sysvararrayput(var a : variant;const value : variant;indexcount : SizeInt;indices : psizeint);cdecl;
  var
    dest : tvardata;
    p : pvararray;
    arraydest : pvariant;
    valuevtype,
    arrayelementtype : tvartype;
    tempvar : variant;
    data : pointer;
    variantmanager : tvariantmanager;
  begin
    dest:=tvardata(a);
    { get final variant }
    while dest.vtype=varByRef or varVariant do
      dest:=tvardata(dest.vpointer^);

    valuevtype:=getfinalvartype(tvardata(value));

    if not(VarTypeIsValidElementType(valuevtype)) and
      { varString isn't a valid varArray type but it is converted
        later }
      (valuevtype<>varString) then
      VarCastError(valuevtype,dest.vtype);

    if (dest.vtype and varArray)<>0 then
      begin
        { get pointer to the array }
        if (dest.vtype and varByRef)<>0 then
          p:=pvararray(dest.vpointer^)
        else
          p:=dest.varray;

        { number of indices ok? }
        if p^.DimCount<>indexcount then
          VarInvalidArgError;

        arrayelementtype:=dest.vtype and vartypemask;
        if arrayelementtype=varVariant then
          begin
            VarResultCheck(SafeArrayPtrOfIndex(p,pvararraycoorarray(indices),arraydest));
            { we can't store ansistrings in variant arrays so we convert the string to
              an olestring }
            if valuevtype=varString then
              begin
                tempvar:=VarToWideStr(value);
                arraydest^:=tempvar;
              end
            else
              arraydest^:=value;
          end
        else
          begin
            GetVariantManager(variantmanager);
            variantmanager.varcast(tempvar,value,arrayelementtype);
            if arrayelementtype in [varOleStr,varDispatch,varUnknown] then
              VarResultCheck(SafeArrayPutElement(p,pvararraycoorarray(indices),tvardata(tempvar).vpointer))
            else
              VarResultCheck(SafeArrayPutElement(p,pvararraycoorarray(indices),@tvardata(tempvar).vpointer));
          end;
      end
    else
      VarInvalidArgError(dest.vtype);
  end;


{ import from system unit }
Procedure fpc_Write_Text_AnsiStr (Len : Longint; Var f : Text; S : AnsiString); external name 'FPC_WRITE_TEXT_ANSISTR';


function syswritevariant(var t : text;const v : variant;width : longint) : Pointer;
  var
    s : ansistring;
    variantmanager : tvariantmanager;
  begin
    GetVariantManager(variantmanager);
    variantmanager.vartolstr(s,v);
    fpc_write_text_ansistr(width,t,s);
  end;


function syswrite0Variant(var t : text;const v : Variant) : Pointer;
  var
    s : ansistring;
    variantmanager : tvariantmanager;
  begin
    getVariantManager(variantmanager);
    variantmanager.vartolstr(s,v);
    fpc_write_text_ansistr(-1,t,s);
  end;

Const
  SysVariantManager : TVariantManager = (
    vartoint      : @sysvartoint;
    vartoint64    : @sysvartoint64;
    vartoword64   : @sysvartoword64;
    vartobool     : @sysvartobool;
    vartoreal     : @sysvartoreal;
    vartotdatetime: @sysvartotdatetime;
    vartocurr     : @sysvartocurr;
    vartopstr     : @sysvartopstr;
    vartolstr     : @sysvartolstr;
    vartowstr     : @sysvartowstr;
    vartointf     : @sysvartointf;
    vartodisp     : @sysvartodisp;
    vartodynarray : @sysvartodynarray;
    varfrombool   : @sysvarfromBool;
    varfromint    : @sysvarfromint;
    varfromint64  : @sysvarfromint64;
    varfromword64 : @sysvarfromword64;
    varfromreal   : @sysvarfromreal;
    varfromtdatetime: @sysvarfromtdatetime;
    varfromcurr   : @sysvarfromcurr;
    varfrompstr   : @sysvarfrompstr;
    varfromlstr   : @sysvarfromlstr;
    varfromwstr   : @sysvarfromwstr;
    varfromintf   : @sysvarfromintf;
    varfromdisp   : @sysvarfromdisp;
    varfromdynarray: @sysvarfromdynarray;
    olevarfrompstr: @sysolevarfrompstr;
    olevarfromlstr: @sysolevarfromlstr;
    olevarfromvar : @sysolevarfromvar;
    olevarfromint : @sysolevarfromint;
    varop         : @sysvarop;
    cmpop         : @syscmpop;
    varneg        : @sysvarneg;
    varnot        : @sysvarnot;
    varinit       : @sysvarinit;
    varclear      : @sysvarclear;
    varaddref     : @sysvaraddref;
    varcopy       : @sysvarcopy;
    varcast       : @sysvarcast;
    varcastole    : @sysvarcastole;
    dispinvoke    : @sysdispinvoke;
    vararrayredim : @sysvararrayredim;
    vararrayget   : @sysvararrayget;
    vararrayput   : @sysvararrayput;
    writevariant  : @syswritevariant;
    write0Variant : @syswrite0variant;
  );

Var
  PrevVariantManager : TVariantManager;

Procedure SetSysVariantManager;

begin
  GetVariantManager(PrevVariantManager);
  SetVariantManager(SysVariantManager);
end;

Procedure UnsetSysVariantManager;

begin
  SetVariantManager(PrevVariantManager);
end;


{ ---------------------------------------------------------------------
   Variant support procedures and functions
  ---------------------------------------------------------------------}


function VarType(const V: Variant): TVarType;

begin
  Result:=TVarData(V).vtype;
end;



function VarAsType(const V: Variant; AVarType: TVarType): Variant;

begin
  sysvarcast(Result,V,AvarType);
end;



function VarIsType(const V: Variant; AVarType: TVarType): Boolean; overload;

begin
  Result:=((TVarData(V).vtype and VarTypeMask)=AVarType);
end;


function VarIsType(const V: Variant; const AVarTypes: array of TVarType): Boolean; overload;

Var
  I : Integer;

begin
  I:=Low(AVarTypes);
  Result:=False;
  While Not Result and (I<=High(AVarTypes)) do
    Result:=((TVarData(V).vtype and VarTypeMask)=AVarTypes[I]);
end;


function VarIsByRef(const V: Variant): Boolean;
begin
  Result:=(TVarData(V).Vtype and varByRef)<>0;
end;


function VarIsEmpty(const V: Variant): Boolean;
begin
  Result:=TVarData(V).vtype=varEmpty;
end;


procedure VarCheckEmpty(const V: Variant);
begin
  If VarIsEmpty(V) Then
    VariantError(SErrVarIsEmpty);
end;


procedure VarClear(var V: Variant);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
begin
  sysvarclear(v);
end;


procedure VarClear(var V: OleVariant);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
begin
  { strange casting using TVarData to avoid call of helper olevariant->variant }
  sysvarclear(Variant(TVarData(v)));
end;


function VarIsNull(const V: Variant): Boolean;
begin
  Result:=TVarData(V).vtype=varNull;
end;


function VarIsClear(const V: Variant): Boolean;

Var
  VT : TVarType;

begin
  VT:=TVarData(V).vtype and varTypeMask;
  Result:=(VT=varEmpty) or
          (((VT=varDispatch) or (VT=VarUnknown))
           and (TVarData(V).VDispatch=Nil));
end;


function VarIsCustom(const V: Variant): Boolean;

begin
  Result:=TVarData(V).vtype>=CFirstUserType;
end;


function VarIsOrdinal(const V: Variant): Boolean;
begin
  Result:=(TVarData(V).VType and varTypeMask) in OrdinalVarTypes;
end;



function VarIsFloat(const V: Variant): Boolean;

begin
  Result:=(TVarData(V).VType and varTypeMask) in FloatVarTypes;
end;


function VarIsNumeric(const V: Variant): Boolean;

begin
  Result:=(TVarData(V).VType and varTypeMask) in (OrdinalVarTypes + FloatVarTypes);
end;



function VarIsStr(const V: Variant): Boolean;

begin
  case (TVarData(V).VType and varTypeMask) of
    varOleStr,
    varString :
      Result:=True;
    else
      Result:=False;
  end;
end;


function VarToStr(const V: Variant): string;

begin
  Result:=VarToStrDef(V,'');
end;


function VarToStrDef(const V: Variant; const ADefault: string): string;

begin
  If TVarData(V).vtype<>varNull then
    Result:=V
  else
    Result:=ADefault;
end;


function VarToWideStr(const V: Variant): WideString;

begin
  Result:=VarToWideStrDef(V,'');
end;


function VarToWideStrDef(const V: Variant; const ADefault: WideString): WideString;

begin
  If TVarData(V).vtype<>varNull then
    Result:=V
  else
    Result:=ADefault;
end;


function VarToDateTime(const V: Variant): TDateTime;

begin
  Result:=VariantToDate(TVarData(V));
end;


function VarFromDateTime(const DateTime: TDateTime): Variant;

begin
  SysVarClear(Result);
  With TVarData(Result) do
    begin
      vtype:=varDate;
      vdate:=DateTime;
    end;
end;


function VarInRange(const AValue, AMin, AMax: Variant): Boolean;
begin
//  Result:=(AValue>=AMin) and (AValue<=AMax);
end;


function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant;
begin
  Result:=AValue;
{ !! Operator not overloaded error...
  If Result>AMAx then
    Result:=AMax
  else If Result<AMin Then
    Result:=AMin;
}
end;


function VarSameValue(const A, B: Variant): Boolean;
  var
    v1,v2 : tvardata;
  begin
    v1:=FindVarData(a)^;
    v2:=FindVarData(b)^;
    if v1.vtype in [VarEmpty,VarNull] then
      result:=v1.vtype=v2.vtype
    else if v2.vtype in [VarEmpty,VarNull] then
      result:=false
    else
      result:=A=B;
  end;


function VarCompareValue(const A, B: Variant): TVariantRelationship;
  var
    v1,v2 : tvardata;
    variantmanager : tvariantmanager;
  begin
    result:=vrNotEqual;
    v1:=FindVarData(a)^;
    v2:=FindVarData(b)^;
    if (v1.vtype in [VarEmpty,VarNull]) and (v1.vtype=v2.vtype) then
      result:=vrEqual
    else if not(v2.vtype in [VarEmpty,VarNull]) and
            not(v1.vtype in [VarEmpty,VarNull]) then
      begin
        if a=b then
          result:=vrEqual
        else if a>b then
          result:=vrGreaterThan
        else
          result:=vrLessThan;
      end;
  end;


function VarIsEmptyParam(const V: Variant): Boolean;
begin
  Result:=(TVarData(V).vtype = varerror) and
          (TVarData(V).verror=VAR_PARAMNOTFOUND);
end;


procedure SetClearVarToEmptyParam(var V: TVarData);
begin
  VariantClear(V);
  V.VType := varError;
  V.VError := VAR_PARAMNOTFOUND;
end;


function VarIsError(const V: Variant; out AResult: HRESULT): Boolean;
begin
end;


function VarIsError(const V: Variant): Boolean;
var
  LResult: HRESULT;
begin
  Result := VarIsError(V, LResult);
end;


function VarAsError(AResult: HRESULT): Variant;
  begin
    tvardata(result).VType:=varError;
    tvardata(result).VError:=AResult;
  end;


function VarSupports(const V: Variant; const IID: TGUID; out Intf): Boolean;
begin
  NotSupported('VarSupports');
end;


function VarSupports(const V: Variant; const IID: TGUID): Boolean;
begin
  NotSupported('VarSupports');
end;


{ Variant copy support }
procedure VarCopyNoInd(var Dest: Variant; const Source: Variant);

begin
  NotSupported('VarCopyNoInd');
end;

{****************************************************************************
              Variant array support procedures and functions
 ****************************************************************************}


function VarArrayCreate(const Bounds: array of SizeInt; AVarType: TVarType): Variant;
  var
    hp : pvararrayboundarray;
    p : pvararray;
    i,lengthb : SizeInt;
  begin
    if not(VarTypeIsValidArrayType(AVarType)) or odd(length(Bounds)) then
      VarArrayCreateError;
    lengthb:=length(Bounds) div 2;
    try
      getmem(hp,lengthb*sizeof(TVarArrayBound));
      for i:=0 to lengthb-1 do
        begin
          hp^[i].lowbound:=Bounds[i*2];
          hp^[i].elementcount:=Bounds[i*2+1]-Bounds[i*2]+1;
        end;
      SysVarClear(result);

      p:=SafeArrayCreate(AVarType,lengthb,hp^);

      if not(assigned(p)) then
        VarArrayCreateError;

      tvardata(result).vtype:=AVarType or varArray;
      tvardata(result).varray:=p;
    finally
      freemem(hp);
    end;
  end;


function VarArrayCreate(const Bounds: pvararrayboundarray; Dims : SizeInt; AVarType: TVarType): Variant;
  var
    p : pvararray;
  begin
    if not(VarTypeIsValidArrayType(AVarType)) then
      VarArrayCreateError;
    SysVarClear(result);

    p:=SafeArrayCreate(AVarType,Dims,Bounds^);

    if not(assigned(p)) then
      VarArrayCreateError;

    tvardata(result).vtype:=AVarType or varArray;
    tvardata(result).varray:=p;
  end;

function VarArrayOf(const Values: array of Variant): Variant;
  var
    i : SizeInt;
  begin
    if length(Values)>0 then
      begin
        result:=VarArrayCreate([0,high(Values)],varVariant);
        for i:=0 to high(Values) do
          result[i]:=Values[i];
      end
    else
      begin
        SysVarClear(result);
        tvardata(result).vtype:=varEmpty;
      end;
  end;


function VarArrayAsPSafeArray(const A: Variant): PVarArray;
  var
    v : tvardata;
  begin
    v:=tvardata(a);
    while v.vtype=varByRef or varVariant do
      v:=tvardata(v.vpointer^);

    if (v.vtype and varArray)=varArray then
      begin
        if (v.vtype and varByRef)<>0 then
          result:=pvararray(v.vpointer^)
        else
          result:=v.varray;
      end
    else
      VarResultCheck(VAR_INVALIDARG);
  end;


function VarArrayDimCount(const A: Variant) : SizeInt;
  var
    hv : tvardata;
  begin
    hv:=tvardata(a);

    { get final variant }
    while hv.vtype=varByRef or varVariant do
      hv:=tvardata(hv.vpointer^);

    if (hv.vtype and varArray)<>0 then
      result:=hv.varray^.DimCount
    else
      result:=0;
  end;


function VarArrayLowBound(const A: Variant; Dim: SizeInt) : SizeInt;
  begin
    VarResultCheck(SafeArrayGetLBound(VarArrayAsPSafeArray(A),Dim,Result));
  end;


function VarArrayHighBound(const A: Variant; Dim: SizeInt) : SizeInt;
  begin
    VarResultCheck(SafeArrayGetUBound(VarArrayAsPSafeArray(A),Dim,Result));
  end;


function VarArrayLock(const A: Variant): Pointer;
  begin
    VarResultCheck(SafeArrayAccessData(VarArrayAsPSafeArray(A),Result));
  end;


procedure VarArrayUnlock(const A: Variant);
  begin
    VarResultCheck(SafeArrayUnaccessData(VarArrayAsPSafeArray(A)));
  end;


function VarArrayRef(const A: Variant): Variant;
  begin
    if (tvardata(a).vtype and varArray)=0 then
      VarInvalidArgError(tvardata(a).vtype);
    tvardata(result).vtype:=tvardata(a).vtype or varByRef;
    if (tvardata(a).vtype and varByRef)=0 then
      tvardata(result).vpointer:=@tvardata(a).varray
    else
      tvardata(result).vpointer:=@tvardata(a).vpointer;
  end;


function VarIsArray(const A: Variant; AResolveByRef: Boolean): Boolean;
  var
    v : tvardata;
  begin
    v:=tvardata(a);
    if AResolveByRef then
      while v.vtype=varByRef or varVariant do
        v:=tvardata(v.vpointer^);

    result:=(v.vtype and varArray)=varArray;
  end;


function VarIsArray(const A: Variant): Boolean;
  begin
    VarIsArray:=VarIsArray(A,true);
  end;


function VarTypeIsValidArrayType(const AVarType: TVarType): Boolean;
  begin
    result:=AVarType in [varsmallint,varinteger,varsingle,vardouble,
      varcurrency,vardate,varolestr,vardispatch,varerror,varboolean,
      varvariant,varunknown,varshortint,varbyte,varword,varlongword];
  end;


function VarTypeIsValidElementType(const AVarType: TVarType): Boolean;
  var
    customvarianttype : TCustomVariantType;
  begin
    if FindCustomVariantType(AVarType,customvarianttype) then
      result:=true
    else
      begin
        result:=(AVarType and not(varByRef)) in [varempty,varnull,varsmallint,varinteger,varsingle,vardouble,
          varcurrency,vardate,varolestr,vardispatch,varerror,varboolean,
          varvariant,varunknown,varshortint,varbyte,varword,varlongword,varint64];
      end;
  end;


{ ---------------------------------------------------------------------
    Variant <-> Dynamic arrays support
  ---------------------------------------------------------------------}

function DynArrayGetVariantInfo(p : pointer;var dims : sizeint) : sizeint;
  begin
    result:=varNull;
    { skip kind and name }
    inc(pointer(p),ord(pdynarraytypeinfo(p)^.namelen)+2);

    p:=aligntoptr(p);

    { skip elesize }
    inc(p,sizeof(sizeint));

    { search recursive? }
    if pdynarraytypeinfo(ppointer(p)^)^.kind=21{tkDynArr} then
      result:=DynArrayGetVariantInfo(ppointer(p)^,dims)
    else
      begin
        { skip dynarraytypeinfo }
        inc(p,sizeof(pdynarraytypeinfo));
        result:=plongint(p)^;
      end;
    inc(dims);
  end;


procedure DynArrayToVariant(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer);
  var
    i,
    dims           : sizeint;
    vararrtype,
    dynarrvartype  : longint;
    vararraybounds : pvararrayboundarray;
    iter : tvariantarrayiter;
    dynarriter : tdynarrayiter;
    p : Pointer;
    temp : variant;
    variantmanager : tvariantmanager;
    dynarraybounds : tdynarraybounds;
  type
    TDynArray = array of pointer;
  begin
    sysvarclearproc(tvardata(v));

    dims:=0;
    dynarrvartype:=DynArrayGetVariantInfo(TypeInfo,dims);

    vararrtype:=dynarrvartype;

    if (dims>1) and not(DynamicArrayIsRectangular(DynArray,TypeInfo)) then
      exit;

    GetVariantManager(variantmanager);

    { retrieve bounds array }
    Setlength(dynarraybounds,dims);
    getmem(vararraybounds,dims*sizeof(TVarArrayBound));
    try
      p:=DynArray;
      for i:=0 to dims-1 do
        begin
          vararraybounds^[i].lowbound:=0;
          vararraybounds^[i].elementcount:=length(TDynArray(p));
          dynarraybounds[i]:=length(TDynArray(p));
          { we checked that the array is rectangular }
          p:=TDynArray(p)[0];
        end;
      { .. create variant array }
      V:=VarArrayCreate(vararraybounds,dims,vararrtype);

      VarArrayLock(V);
      try
        iter.init(dims,pvararrayboundarray(vararraybounds));
        dynarriter.init(DynArray,TypeInfo,dims,dynarraybounds);
        repeat
          case vararrtype of
            varsmallint:
              temp:=PSmallInt(dynarriter.data)^;
            varinteger:
              temp:=PInteger(dynarriter.data)^;
            varsingle:
              temp:=PSingle(dynarriter.data)^;
            vardouble:
              temp:=PDouble(dynarriter.data)^;
            varcurrency:
              temp:=PCurrency(dynarriter.data)^;
            vardate:
              temp:=PDouble(dynarriter.data)^;
            varolestr:
              temp:=PWideString(dynarriter.data)^;
            vardispatch:
              temp:=PDispatch(dynarriter.data)^;
            varerror:
              temp:=PError(dynarriter.data)^;
            varboolean:
              temp:=PBoolean(dynarriter.data)^;
            varvariant:
              temp:=PVariant(dynarriter.data)^;
            varunknown:
              temp:=PUnknown(dynarriter.data)^;
            varshortint:
              temp:=PShortInt(dynarriter.data)^;
            varbyte:
              temp:=PByte(dynarriter.data)^;
            varword:
              temp:=PWord(dynarriter.data)^;
            varlongword:
              temp:=PLongWord(dynarriter.data)^;
            varint64:
              temp:=PInt64(dynarriter.data)^;
            varqword:
              temp:=PQWord(dynarriter.data)^;
            else
              VarClear(temp);
          end;
          dynarriter.next;
          variantmanager.VarArrayPut(V,temp,dims,PSizeInt(iter.coords));
        until not(iter.next);
      finally
        iter.done;
        dynarriter.done;
        VarArrayUnlock(V);
      end;
    finally
      freemem(vararraybounds);
    end;
  end;


procedure DynArrayFromVariant(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer);
  var
    DynArrayDims,
    VarArrayDims : SizeInt;
    iter : tvariantarrayiter;
    dynarriter : tdynarrayiter;
    temp : variant;
    dynarrvartype : longint;
    variantmanager : tvariantmanager;
    vararraybounds : pvararrayboundarray;
    dynarraybounds : tdynarraybounds;
    i : SizeInt;
    p : Pointer;
  type
    TDynArray = array of pointer;
  begin
    VarArrayDims:=VarArrayDimCount(V);

    DynArrayDims:=0;
    dynarrvartype:=DynArrayGetVariantInfo(TypeInfo,DynArrayDims);

    if (VarArrayDims=0) or (VarArrayDims<>DynArrayDims) then
      VarResultCheck(VAR_INVALIDARG);

    { retrieve bounds array }
    Setlength(dynarraybounds,VarArrayDims);
    getmem(vararraybounds,VarArrayDims*sizeof(TVarArrayBound));
    try
      p:=DynArray;
      for i:=0 to VarArrayDims-1 do
        begin
          vararraybounds^[i].lowbound:=VarArrayLowBound(V,i+1);
          vararraybounds^[i].elementcount:=VarArrayHighBound(V,i+1)-vararraybounds^[i].lowbound+1;
          dynarraybounds[i]:=vararraybounds^[i].elementcount;
        end;
      DynArraySetLength(DynArray,TypeInfo,VarArrayDims,PSizeInt(dynarraybounds));
      GetVariantManager(variantmanager);
      VarArrayLock(V);
      try
        iter.init(VarArrayDims,pvararrayboundarray(vararraybounds));
        dynarriter.init(DynArray,TypeInfo,VarArrayDims,dynarraybounds);
        repeat
          temp:=variantmanager.VarArrayGet(V,VarArrayDims,PSizeInt(iter.coords));
          case dynarrvartype of
            varsmallint:
              PSmallInt(dynarriter.data)^:=temp;
            varinteger:
              PInteger(dynarriter.data)^:=temp;
            varsingle:
              PSingle(dynarriter.data)^:=temp;
            vardouble:
              PDouble(dynarriter.data)^:=temp;
            varcurrency:
              PCurrency(dynarriter.data)^:=temp;
            vardate:
              PDouble(dynarriter.data)^:=temp;
            varolestr:
              PWideString(dynarriter.data)^:=temp;
            vardispatch:
              PDispatch(dynarriter.data)^:=temp;
            varerror:
              PError(dynarriter.data)^:=temp;
            varboolean:
              PBoolean(dynarriter.data)^:=temp;
            varvariant:
              PVariant(dynarriter.data)^:=temp;
            varunknown:
              PUnknown(dynarriter.data)^:=temp;
            varshortint:
              PShortInt(dynarriter.data)^:=temp;
            varbyte:
              PByte(dynarriter.data)^:=temp;
            varword:
              PWord(dynarriter.data)^:=temp;
            varlongword:
              PLongWord(dynarriter.data)^:=temp;
            varint64:
              PInt64(dynarriter.data)^:=temp;
            varqword:
              PQWord(dynarriter.data)^:=temp;
          end;
          dynarriter.next;
        until not(iter.next);
      finally
        iter.done;
        dynarriter.done;
        VarArrayUnlock(V);
      end;
    finally
      freemem(vararraybounds);
    end;
  end;


function FindCustomVariantType(const AVarType: TVarType; out CustomVariantType: TCustomVariantType): Boolean; overload;
  begin
    result:=assigned(customvarianttype) and (AVarType>=CMinVarType);
    if result then
      begin
        EnterCriticalSection(customvarianttypelock);
        try
          result:=(AVarType-CMinVarType)<=high(customvarianttypes);
          if result then
            begin
              CustomVariantType:=customvarianttypes[AVarType-CMinVarType];
              result:=assigned(CustomVariantType) and
               (CustomVariantType<>InvalidCustomVariantType);
            end;
        finally
          LeaveCriticalSection(customvarianttypelock);
        end;
      end;
  end;


function FindCustomVariantType(const TypeName: string;  out CustomVariantType: TCustomVariantType): Boolean; overload;

begin
  NotSupported('FindCustomVariantType');
end;

function Unassigned: Variant; // Unassigned standard constant
begin
  SysVarClear(Result);
  TVarData(Result).VType := varempty;
end;


function Null: Variant;       // Null standard constant
  begin
    SysVarClear(Result);
    TVarData(Result).VType := varnull;
  end;


{ ---------------------------------------------------------------------
    TCustomVariantType Class.
  ---------------------------------------------------------------------}

function TCustomVariantType.QueryInterface(const IID: TGUID; out Obj): HResult;  stdcall;
  begin
    NotSupported('TCustomVariantType.QueryInterface');
  end;


function TCustomVariantType._AddRef: Integer; stdcall;
  begin
    NotSupported('TCustomVariantType._AddRef');
  end;


function TCustomVariantType._Release: Integer; stdcall;
  begin
    NotSupported('TCustomVariantType._Release');
  end;


procedure TCustomVariantType.SimplisticClear(var V: TVarData);
  begin
    NotSupported('TCustomVariantType.SimplisticClear');
  end;


procedure TCustomVariantType.SimplisticCopy(var Dest: TVarData; const Source: TVarData;  const Indirect: Boolean = False);
begin
  NotSupported('TCustomVariantType.SimplisticCopy');
end;


procedure TCustomVariantType.RaiseInvalidOp;
begin
  NotSupported('TCustomVariantType.RaiseInvalidOp');
end;


procedure TCustomVariantType.RaiseCastError;
begin
  NotSupported('TCustomVariantType.RaiseCastError');
end;


procedure TCustomVariantType.RaiseDispError;

begin
  NotSupported('TCustomVariantType.RaiseDispError');
end;



function TCustomVariantType.LeftPromotion(const V: TVarData; const Operation: TVarOp; out RequiredVarType: TVarType): Boolean;

begin
  NotSupported('TCustomVariantType.LeftPromotion');
end;


function TCustomVariantType.RightPromotion(const V: TVarData; const Operation: TVarOp;  out RequiredVarType: TVarType): Boolean;

begin
  NotSupported('TCustomVariantType.RightPromotion');
end;


function TCustomVariantType.OlePromotion(const V: TVarData;  out RequiredVarType: TVarType): Boolean;

begin
  NotSupported('TCustomVariantType.OlePromotion');
end;


procedure TCustomVariantType.DispInvoke(var Dest: TVarData; const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);

begin
  NotSupported('TCustomVariantType.DispInvoke');
end;



procedure TCustomVariantType.VarDataInit(var Dest: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataInit');
end;


procedure TCustomVariantType.VarDataClear(var Dest: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataClear');
end;



procedure TCustomVariantType.VarDataCopy(var Dest: TVarData; const Source: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataCopy');
end;


procedure TCustomVariantType.VarDataCopyNoInd(var Dest: TVarData; const Source: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataCopyNoInd');
end;



procedure TCustomVariantType.VarDataCast(var Dest: TVarData; const Source: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataCast');
end;


procedure TCustomVariantType.VarDataCastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);

begin
  NotSupported('TCustomVariantType.VarDataCastTo');
end;


procedure TCustomVariantType.VarDataCastTo(var Dest: TVarData; const AVarType: TVarType);

begin
  NotSupported('TCustomVariantType.VarDataCastTo');
end;


procedure TCustomVariantType.VarDataCastToOleStr(var Dest: TVarData);

begin
  NotSupported('TCustomVariantType.VarDataCastToOleStr');
end;



procedure TCustomVariantType.VarDataFromStr(var V: TVarData; const Value: string);

begin
  NotSupported('TCustomVariantType.VarDataFromStr');
end;


procedure TCustomVariantType.VarDataFromOleStr(var V: TVarData; const Value: WideString);

begin
  NotSupported('TCustomVariantType.VarDataFromOleStr');
end;


function TCustomVariantType.VarDataToStr(const V: TVarData): string;

begin
  NotSupported('TCustomVariantType.VarDataToStr');
end;



function TCustomVariantType.VarDataIsEmptyParam(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsEmptyParam');
end;


function TCustomVariantType.VarDataIsByRef(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsByRef');
end;


function TCustomVariantType.VarDataIsArray(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsArray');
end;



function TCustomVariantType.VarDataIsOrdinal(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsOrdinal');
end;


function TCustomVariantType.VarDataIsFloat(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsFloat');
end;


function TCustomVariantType.VarDataIsNumeric(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsNumeric');
end;


function TCustomVariantType.VarDataIsStr(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.VarDataIsStr');
end;


constructor TCustomVariantType.Create;

begin
  NotSupported('TCustomVariantType.Create;');
end;


constructor TCustomVariantType.Create(RequestedVarType: TVarType);

begin
  NotSupported('TCustomVariantType.Create');
end;


destructor TCustomVariantType.Destroy;

begin
  NotSupported('TCustomVariantType.Destroy');
end;



function TCustomVariantType.IsClear(const V: TVarData): Boolean;

begin
  NotSupported('TCustomVariantType.IsClear');
end;


procedure TCustomVariantType.Cast(var Dest: TVarData; const Source: TVarData);

begin
  NotSupported('TCustomVariantType.Cast');
end;


procedure TCustomVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);

begin
  NotSupported('TCustomVariantType.CastTo');
end;


procedure TCustomVariantType.CastToOle(var Dest: TVarData; const Source: TVarData);

begin
  NotSupported('TCustomVariantType.CastToOle');
end;



procedure TCustomVariantType.BinaryOp(var Left: TVarData; const Right: TVarData; const Operation: TVarOp);

begin
  NotSupported('TCustomVariantType.BinaryOp');
end;


procedure TCustomVariantType.UnaryOp(var Right: TVarData; const Operation: TVarOp);

begin
  NotSupported('TCustomVariantType.UnaryOp');
end;


function TCustomVariantType.CompareOp(const Left, Right: TVarData; const Operation: TVarOp): Boolean;

begin
  NotSupported('TCustomVariantType.CompareOp');
end;


procedure TCustomVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);

begin
  NotSupported('TCustomVariantType.Compare');
end;

{ ---------------------------------------------------------------------
    TInvokeableVariantType implementation
  ---------------------------------------------------------------------}

procedure TInvokeableVariantType.DispInvoke(var Dest: TVarData; const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);

begin
  NotSupported('TInvokeableVariantType.DispInvoke');
end;

function TInvokeableVariantType.DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;

begin
  NotSupported('TInvokeableVariantType.DoFunction');
end;

function TInvokeableVariantType.DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  NotSupported('TInvokeableVariantType.DoProcedure');
end;


function TInvokeableVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
  begin
    NotSupported('TInvokeableVariantType.GetProperty');
  end;


function TInvokeableVariantType.SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean;
  begin
    NotSupported('TInvokeableVariantType.SetProperty');
  end;


function TPublishableVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
  begin
    result:=true;
    variant(dest):=GetPropValue(getinstance(v),name);
  end;


function TPublishableVariantType.SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean;
  begin
    result:=true;
    SetPropValue(getinstance(v),name,variant(value));
  end;


procedure VarCastError;
  begin
    raise EVariantTypeCastError.Create(SInvalidVarCast);
  end;


procedure VarCastError(const ASourceType, ADestType: TVarType);
  begin
    raise EVariantTypeCastError.CreateFmt(SVarTypeCouldNotConvert,
      [VarTypeAsText(ASourceType),VarTypeAsText(ADestType)]);
  end;


procedure VarInvalidOp;
  begin
    raise EVariantInvalidOpError.Create(SInvalidVarOp);
  end;


procedure VarInvalidNullOp;
  begin
    raise EVariantInvalidOpError.Create(SInvalidVarNullOp);
  end;


procedure VarParamNotFoundError;
  begin
    raise EVariantParamNotFoundError.Create(SVarParamNotFound);
  end;


procedure VarBadTypeError;
  begin
    raise EVariantBadVarTypeError.Create(SVarBadType);
  end;


procedure VarOverflowError;
  begin
    raise EVariantOverflowError.Create(SVarOverflow);
  end;


procedure VarOverflowError(const ASourceType, ADestType: TVarType);
  begin
    raise EVariantOverflowError.CreateFmt(SVarTypeConvertOverflow,
      [VarTypeAsText(ASourceType),VarTypeAsText(ADestType)]);
  end;


procedure VarRangeCheckError(const AType: TVarType);
  begin
    raise EVariantOverflowError.CreateFmt(SVarTypeRangeCheck1,
      [VarTypeAsText(AType)])
  end;


procedure VarRangeCheckError(const ASourceType, ADestType: TVarType);
  begin
    if ASourceType<>ADestType then
      raise EVariantOverflowError.CreateFmt(SVarTypeRangeCheck2,
        [VarTypeAsText(ASourceType),VarTypeAsText(ADestType)])
    else
      VarRangeCheckError(ASourceType);
  end;


procedure VarBadIndexError;
  begin
    raise EVariantBadIndexError.Create(SVarArrayBounds);
  end;


procedure VarArrayLockedError;
  begin
    raise EVariantArrayLockedError.Create(SVarArrayLocked);
  end;


procedure VarNotImplError;
  begin
    raise EVariantNotImplError.Create(SVarNotImplemented);
  end;


procedure VarOutOfMemoryError;
  begin
    raise EVariantOutOfMemoryError.Create(SOutOfMemory);
  end;


procedure VarInvalidArgError;
  begin
    raise EVariantInvalidArgError.Create(SVarInvalid);
  end;


procedure VarInvalidArgError(AType: TVarType);
  begin
    raise EVariantInvalidArgError.CreateFmt(SVarInvalid1,
      [VarTypeAsText(AType)])
  end;


procedure VarUnexpectedError;
  begin
    raise EVariantUnexpectedError.Create(SVarUnexpected);
  end;


procedure VarArrayCreateError;
  begin
    raise EVariantArrayCreateError.Create(SVarArrayCreate);
  end;


procedure RaiseVarException(res : HRESULT);
  begin
    case res of
      VAR_PARAMNOTFOUND:
        VarParamNotFoundError;
      VAR_TYPEMISMATCH:
        VarCastError;
      VAR_BADVARTYPE:
        VarBadTypeError;
      VAR_EXCEPTION:
        VarInvalidOp;
      VAR_OVERFLOW:
        VarOverflowError;
      VAR_BADINDEX:
        VarBadIndexError;
      VAR_ARRAYISLOCKED:
        VarArrayLockedError;
      VAR_NOTIMPL:
        VarNotImplError;
      VAR_OUTOFMEMORY:
        VarOutOfMemoryError;
      VAR_INVALIDARG:
        VarInvalidArgError;
      VAR_UNEXPECTED:
        VarUnexpectedError;
      else
        raise EVariantError.CreateFmt(SInvalidVarOpWithHResultWithPrefix,
          ['$',res,'']);
    end;
  end;


procedure VarResultCheck(AResult: HRESULT);{$ifdef VARIANTINLINE}inline;{$endif VARIANTINLINE}
  begin
    if AResult<>VAR_OK then
      RaiseVarException(AResult);
  end;


procedure VarResultCheck(AResult: HRESULT; ASourceType, ADestType: TVarType);
  begin
    case AResult of
      VAR_OK:
        ;
      VAR_OVERFLOW:
        VarOverflowError(ASourceType,ADestType);
      VAR_TYPEMISMATCH:
        VarCastError(ASourceType,ADestType);
    else
      RaiseVarException(AResult);
    end;
  end;


procedure HandleConversionException(const ASourceType, ADestType: TVarType);
  begin
    if exceptobject is econverterror then
      varcasterror(asourcetype,adesttype)
    else if (exceptobject is eoverflow) or
      (exceptobject is erangeerror) then
      varoverflowerror(asourcetype,adesttype)
    else
      raise exception(acquireexceptionobject);
  end;


function VarTypeAsText(const AType: TVarType): string;
  var
    customvarianttype : tcustomvarianttype;
  const
    names : array[varempty..varqword] of string[8] = (
    'Empty','Null','Smallint','Integer','Single','Double','Currency','Date','OleStr','Dispatch','Error','Boolean','Variant',
    'Unknown','Decimal','???','ShortInt','Byte','Word','DWord','Int64','QWord');
  begin
    if ((AType and VarTypeMask)>=low(names)) and ((AType and VarTypeMask)<=high(names)) then
      result:=names[AType]
    else
      case AType and VarTypeMask of
        varString:
          result:='String';
        varAny:
          result:='Any';
        else
          begin
            if FindCustomVariantType(AType and VarTypeMask,customvarianttype) then
              result:=customvarianttype.classname
            else
              result:='$'+IntToHex(AType and VarTypeMask,4)
          end;
      end;
    if (AType and vararray)<>0 then
      result:='Array of '+result;
    if (AType and varbyref)<>0 then
      result:='Ref to '+result;
  end;


function FindVarData(const V: Variant): PVarData;
  begin
    result:=@V;
    while result^.vtype=varVariant or VarByRef do
      result:=PVarData(result^.VPointer);
  end;

{ ---------------------------------------------------------------------
    Variant properties from typinfo
  ---------------------------------------------------------------------}


Function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
begin
{$warning GetVariantProp not implemented}
  Result:=Null;
end;


Procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo; const Value: Variant);
begin
{$warning SetVariantProp not implemented}
end;


Function GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetVariantProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetVariantProp(Instance: TObject; const PropName: string;  const Value: Variant);
begin
  SetVariantprop(instance,FindpropInfo(Instance,PropName),Value);
end;

{ ---------------------------------------------------------------------
  All properties through variant.
  ---------------------------------------------------------------------}

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetPropValue(Instance,PropName,True);
end;


Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;

var
  PropInfo: PPropInfo;

begin
  // find the property
  PropInfo := GetPropInfo(Instance, PropName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName])
 else
   begin
   Result := Null; //at worst
   // call the right GetxxxProp
   case PropInfo^.PropType^.Kind of
     tkInteger, tkChar, tkWChar, tkClass, tkBool:
        Result := GetOrdProp(Instance, PropInfo);
     tkEnumeration:
     if PreferStrings then
       Result := GetEnumProp(Instance, PropInfo)
     else
       Result := GetOrdProp(Instance, PropInfo);
     tkSet:
       if PreferStrings then
         Result := GetSetProp(Instance, PropInfo, False)
       else
         Result := GetOrdProp(Instance, PropInfo);
     tkFloat:
       Result := GetFloatProp(Instance, PropInfo);
     tkMethod:
       Result := PropInfo^.PropType^.Name;
     tkString, tkLString, tkAString:
       Result := GetStrProp(Instance, PropInfo);
     tkWString:
       Result := GetWideStrProp(Instance, PropInfo);
     tkVariant:
       Result := GetVariantProp(Instance, PropInfo);
     tkInt64:
       Result := GetInt64Prop(Instance, PropInfo);
   else
     raise EPropertyError.CreateFmt('Invalid Property Type: %s',[PropInfo^.PropType^.Name]);
   end;
   end;
end;

Procedure SetPropValue(Instance: TObject; const PropName: string;  const Value: Variant);

var
 PropInfo: PPropInfo;
 TypeData: PTypeData;

begin
   // find the property
   PropInfo := GetPropInfo(Instance, PropName);
   if PropInfo = nil then
     raise EPropertyError.CreateFmt('SetPropValue: Unknown property: "%s"', [PropName])
   else
     begin
     TypeData := GetTypeData(PropInfo^.PropType);
     // call right SetxxxProp
     case PropInfo^.PropType^.Kind of
       tkInteger, tkChar, tkWChar, tkBool, tkEnumeration, tkSet:
         SetOrdProp(Instance, PropInfo, Value);
       tkFloat:
         SetFloatProp(Instance, PropInfo, Value);
       tkString, tkLString, tkAString:
         SetStrProp(Instance, PropInfo, VarToStr(Value));
       tkWString:
         SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
       tkVariant:
         SetVariantProp(Instance, PropInfo, Value);
       tkInt64:
         SetInt64Prop(Instance, PropInfo, Value);
     else
       raise EPropertyError.CreateFmt('SetPropValue: Invalid Property Type %s',
                                      [PropInfo^.PropType^.Name]);
     end;
   end;
end;


Initialization
  InitCriticalSection(customvarianttypelock);
  SetSysVariantManager;
  SetClearVarToEmptyParam(TVarData(EmptyParam));
  VarClearProc:=@sysvarclearproc;
  VarAddRefProc:=@sysvaraddrefproc;
  VarCopyProc:=@sysvarcopyproc;
  // Typinfo variant support
  OnGetVariantProp:=@GetVariantprop;
  OnSetVariantProp:=@SetVariantprop;
  OnSetPropValue:=@SetPropValue;
  OnGetPropValue:=@GetPropValue;
  InvalidCustomVariantType:=TCustomVariantType(-1);
Finalization
  UnSetSysVariantManager;
  DoneCriticalSection(customvarianttypelock);

end.
