{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Const
  // Ratio of Pointer and Word Size.
  WordRatio = SizeOf(Pointer) Div SizeOf(Word);

{****************************************************************************}
{*                           TFPList                                        *}
{****************************************************************************}

procedure TFPList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;

function TFPList.Get(Index: Integer): Pointer; {$ifdef HASINLINE} inline; {$endif}
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FList^[Index];
end;

procedure TFPList.Put(Index: Integer; Item: Pointer); {$ifdef HASINLINE} inline; {$endif}
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Flist^[Index] := Item;
end;

function TFPList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  result := nil;
  i := IndexOf(item);
  if i >= 0 then
   begin
     Result := item;
     FList^[i] := nil;
     Delete(i);
   end;
end;

procedure TFPList.SetCapacity(NewCapacity: Integer);
begin
  If (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    exit;
  ReallocMem(FList, SizeOf(Pointer)*NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TFPList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize)then
    Error(SListCountError, NewCount);
  If NewCount > FCount then
    begin
    If NewCount > FCapacity then
      SetCapacity(NewCount);
    If FCount < NewCount then
      FillWord(Flist^[FCount], (NewCount-FCount) *  WordRatio, 0);
    end;
  FCount := Newcount;
end;

destructor TFPList.Destroy;
begin
  Self.Clear;
  inherited Destroy;
end;

function TFPList.Add(Item: Pointer): Integer; {$ifdef HASINLINE} inline; {$endif}
begin
  if FCount = FCapacity then 
    Self.Expand;
  FList^[FCount] := Item;
  Result := FCount;
  FCount := FCount + 1;
end;

procedure TFPList.Clear;
begin
  if Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
    FList := nil;
  end;
end;

procedure TFPList.Delete(Index: Integer); {$ifdef HASINLINE} inline; {$endif}
begin
  If (Index<0) or (Index>=FCount) then
    Error (SListIndexError, Index);
  FCount := FCount-1;
  System.Move (FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
  // Shrink the list if appropriate
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
  begin
    FCapacity := FCapacity shr 1;
    ReallocMem(FList, SizeOf(Pointer) * FCapacity);
  end;
end;

class procedure TFPList.Error(const Msg: string; Data: PtrInt);
begin
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
end;

procedure TFPList.Exchange(Index1, Index2: Integer);
var
  Temp : Pointer;
begin
  If ((Index1 >= FCount) or (Index1 < 0)) then
    Error(SListIndexError, Index1);
  If ((Index2 >= FCount) or (Index2 < 0)) then
    Error(SListIndexError, Index2);
  Temp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Temp;
end;

function TFPList.Expand: TFPList;  {$ifdef HASINLINE} inline; {$endif}
var
  IncSize : Longint;
begin
  if FCount < FCapacity then exit;
  IncSize := 4;
  if FCapacity > 3 then IncSize := IncSize + 4;
  if FCapacity > 8 then IncSize := IncSize+8;
  if FCapacity > 127 then Inc(IncSize, FCapacity shr 2);
  SetCapacity(FCapacity + IncSize);
  Result := Self;
end;

function TFPList.First: Pointer;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := Items[0];
end;

function TFPList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while(Result < FCount) and (Flist^[Result] <> Item) do Result := Result + 1;
  If Result = FCount  then Result := -1;
end;

procedure TFPList.Insert(Index: Integer; Item: Pointer);  {$ifdef HASINLINE} inline; {$endif}
begin
  if (Index < 0) or (Index > FCount )then
    Error(SlistIndexError, Index);
  iF FCount = FCapacity then Self.Expand;
  if Index<FCount then
    System.Move(Flist^[Index], Flist^[Index+1], (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  FCount := FCount + 1;
end;

function TFPList.Last: Pointer;
begin
{ Wouldn't it be better to return nil if the count is zero ?}
  If FCount = 0 then
    Result := nil
  else
    Result := Items[FCount - 1];
end;

procedure TFPList.Move(CurIndex, NewIndex: Integer);
var
  Temp : Pointer;
begin
  if ((CurIndex < 0) or (CurIndex > Count - 1)) then
    Error(SListIndexError, CurIndex);
  if (NewINdex < 0) then
    Error(SlistIndexError, NewIndex);
  Temp := FList^[CurIndex];
  FList^[CurIndex] := nil;
  Self.Delete(CurIndex);
  Self.Insert(NewIndex, nil);
  FList^[NewIndex] := Temp;
end;

function TFPList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

procedure TFPList.Pack;
Var
  {Last,I,J,}
  Runner : Longint;
begin
  // Not the fastest; but surely correct
  for Runner := Fcount - 1 downto 0 do
    if Items[Runner] = Nil then
      Self.Delete(Runner);
{ The following may be faster in case of large and defragmented lists
  If count=0 then exit;
  Runner:=0;I:=0;
  TheLast:=Count;
  while runner<count do
    begin
    // Find first Nil
    While (FList^[Runner]<>Nil) and (Runner<Count) do Runner:=Runner+1;
    if Runner<Count do
      begin
      // Start searching for non-nil from last known nil+1
      if i<Runner then I:=Runner+1;
      While (Flist[I]^=Nil) and (I<Count) do I:=I+1;
      // Start looking for last non-nil of block.
      J:=I+1;
      While (Flist^[J]<>Nil) and (J<Count) do J:=J+1;
      // Move block and zero out
      Move (Flist^[I],Flist^[Runner],J*SizeOf(Pointer));
      FillWord (Flist^[I],(J-I)*WordRatio,0);
      // Update Runner and Last to point behind last block
      TheLast:=Runner+(J-I);
      If J=Count then
         begin
         // Shortcut, when J=Count we checked all pointers
         Runner:=Count
      else
         begin
         Runner:=TheLast;
         I:=j;
      end;
    end;
  Count:=TheLast;
}
end;

// Needed by Sort method.

Procedure QuickSort(FList: PPointerList; L, R : Longint;
                     Compare: TListSortCompare);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while Compare(P, FList^[i]) > 0 do
       I := I + 1;
     while Compare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     QuickSort(FList, L, J, Compare);
   L := I;
 until I >= R;
end;

procedure TFPList.Sort(Compare: TListSortCompare);
begin
  if Not Assigned(FList) or (FCount < 2) then exit;
  QuickSort(Flist, 0, FCount-1, Compare);
end;

procedure TFPList.Assign(Obj: TFPList);
var
  i: Integer;
begin
  Clear;
  for I := 0 to Obj.Count - 1 do
    Add(Obj[i]);
end;


{****************************************************************************}
{*                TList                                        *}
{****************************************************************************}

{  TList = class(TObject)
  private
    FList: TFPList;
}



function TList.Get(Index: Integer): Pointer;
begin
  Result := FList.Get(Index);
end;

procedure TList.Grow;
begin
  // Only for compatibility with Delphi. Not needed.
end;

procedure TList.Put(Index: Integer; Item: Pointer);
begin
  FList.Put(Index, Item);
end;

function TList.Extract(item: Pointer): Pointer;
begin
  Result := FList.Extract(item);
  Notify(Result, lnExtracted);
end;

procedure TList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

function TList.GetCapacity: integer;
begin
  Result := FList.Capacity;
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  FList.SetCapacity(NewCapacity);
end;

function TList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TList.SetCount(NewCount: Integer);
begin
  FList.SetCount(NewCount);
end;

constructor TList.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TList.Destroy;
begin
  If (Flist<>Nil) then
    Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TList.Add(Item: Pointer): Integer; 
begin
  Result := FList.Add(Item);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TList.Clear; 

begin
  If Assigned(Flist) then
    While (FList.Count>0) do
      Delete(Count-1);
end;

procedure TList.Delete(Index: Integer); 

var P : pointer;

begin
  P:=FList.Get(Index);
  FList.Delete(Index);
  if assigned(p) then Notify(p, lnDeleted);
end;

class procedure TList.Error(const Msg: string; Data: Integer);
begin
  Raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
end;

procedure TList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TList.Expand: TList;
begin
  FList.Expand;
  Result:=Self;
end;

function TList.First: Pointer;
begin
  Result := FList.First;
end;

function TList.IndexOf(Item: Pointer): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TList.Insert(Index: Integer; Item: Pointer); 
begin
  FList.Insert(Index, Item);
  if Item <> nil then
    Notify(Item,lnAdded);
end;

function TList.Last: Pointer;
begin
  Result := FList.Last;
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

procedure TList.Pack;
begin
  FList.Pack;
end;

procedure TList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

procedure TList.Assign(Obj:TList);
begin
  FList.Assign(Obj.FList);
end;

function TList.GetList: PPointerList;
begin
  Result := FList.List;
end;


{****************************************************************************}
{*                             TThreadList                                  *}
{****************************************************************************}


constructor TThreadList.Create;
  begin
    inherited Create;
    FDuplicates:=dupIgnore;
    InitCriticalSection(FLock);
    FList:=TList.Create;
  end;


destructor TThreadList.Destroy;
  begin
    LockList;
    try    
      FList.Free;
      inherited Destroy;
    finally
      UnlockList;
      DoneCriticalSection(FLock);
    end;  
  end;



procedure TThreadList.Add(Item: Pointer);
  begin
    LockList;
    try      
      if (Duplicates=dupAccept) or
        // make sure it's not already in the list
        (FList.IndexOf(Item)=-1) then
         FList.Add(Item)
       else if (Duplicates=dupError) then
         FList.Error(SDuplicateItem,PtrInt(Item));
    finally
      UnlockList;
    end;
  end;


procedure TThreadList.Clear;
  begin
    Locklist;
    try
      FList.Clear;
    finally
      UnLockList;
    end;
  end;


function TThreadList.LockList: TList;
  begin  
    Result:=FList;
    EnterCriticalSection(FLock);
  end;


procedure TThreadList.Remove(Item: Pointer);
  begin
    LockList;
    try
      FList.Remove(Item);
    finally
      UnlockList;
    end;
  end;


procedure TThreadList.UnlockList;
  begin
    LeaveCriticalSection(FLock);
  end;


{
  $Log: lists.inc,v $
  Revision 1.11  2005/05/04 06:53:06  michael
  * Removed TFPList.grow

  Revision 1.10  2005/04/12 10:03:31  marco
   * notifyer triggered after delete patch

  Revision 1.9  2005/03/16 22:56:05  michael
  + Fix for TList.Destroy and improved Clear

  Revision 1.8  2005/03/16 20:48:03  michael
  + Faster TList from Dean Zobec

  Revision 1.7  2005/02/14 17:13:31  peter
    * truncate log

}
