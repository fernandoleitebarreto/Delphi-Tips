// ***************************************************************************
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Updated by Wuping Xin Copyright (c) 2020
// ***************************************************************************

unit EventBus.Utils.DuckList;

interface

uses
  Generics.Collections,
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.TypInfo;

type
  TDuckTypedList = class;

  TDormObjectStatus = (
    Dirty = 0,
    Clean,
    Unknown,
    Deleted
  );

  EDormException = class(Exception)
  end;

  EDormValidationException = class(EDormException)
  end;

  TDormEnvironment = (
    Development,
    Test,
    Release
  );

  TDormObjectOwner = (
    Itself,
    Parent
  );

  TDormSaveKind = (
    AllGraph,
    SingleObject
  );

  TDormRelations = set of (
    BelongsTo,
    HasMany,
    HasOne
  );

  TDormFillOptions = set of (
    CallAfterLoadEvent
  );

  TDormListEnumerator = class(TEnumerator<TObject>)
  protected
    FDuckTypedList: TDuckTypedList;
    FPosition: Int64;
    function DoGetCurrent: TObject; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(ADuckTypedList: TDuckTypedList);
  end;

  TSortingDirection = (
    Ascending,
    Descending
  );

  IWrappedList = interface
    ['{B60AF5A6-7C31-4EAA-8DFB-D8BD3E112EE7}']
    procedure Add(const AObject: TObject);
    procedure Clear;
    function Count: Integer;
    function GetEnumerator: TDormListEnumerator;
    function GetItem(const AItemIndex: Integer): TObject;
    procedure Sort(const APropertyName: string; ADirection: TSortingDirection = Ascending);
    function WrappedObject: TObject;
    function get_OwnsObjects: boolean;
    procedure set_OwnsObjects(const Value: boolean);
    property OwnsObjects: boolean read get_OwnsObjects write set_OwnsObjects;
  end;

  TDuckTypedList = class(TInterfacedObject, IWrappedList)
  private
    function get_OwnsObjects: Boolean;
    procedure set_OwnsObjects(const Value: Boolean);
  protected
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetCountMethod: TRttiMethod;
    FGetItemMethod: TRttiMethod;
    FObjectAsDuck: TObject;
    FRttiContext: TRttiContext;
    procedure Add(const AObj: TObject);
    procedure Clear;
    function Count: Integer;
    function GetItem(const AItemIndex: Integer): TObject;
    procedure QuickSort(AList: IWrappedList; L, R: Integer; AComparer: TFunc<TObject, TObject, Integer>); overload;
    procedure QuickSort(AList: IWrappedList; AComparer: TFunc<TObject, TObject, Integer>); overload;
    procedure Sort(const APropName: string; ADirection: TSortingDirection = Ascending);
    function WrappedObject: TObject;
  public
    constructor Create(AObjectAsDuck: TObject);
    destructor Destroy; override;
    class function CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean;
    function GetEnumerator: TDormListEnumerator;
    property OwnsObjects: Boolean read get_OwnsObjects write set_OwnsObjects;
  end;

function WrapAsList(const AObject: TObject): IWrappedList;

implementation

uses
  System.Math,
  EventBus.Utils.Rtti;

function WrapAsList(const AObject: TObject): IWrappedList;
begin
  try
    Result := TDuckTypedList.Create(AObject);
  except
    Result := nil;
  end;
end;

function CompareValue(const Left, Right: TValue): Integer;
begin
  if Left.IsOrdinal then
    Result := System.Math.CompareValue(Left.AsOrdinal, Right.AsOrdinal)
  else if Left.Kind = tkFloat then
    Result := System.Math.CompareValue(Left.AsExtended, Right.AsExtended)
  else if Left.Kind in [tkString, tkUString, tkWString, tkLString] then
    Result := CompareText(Left.AsString, Right.AsString)
  else
    Result := 0;
end;

constructor TDormListEnumerator.Create(ADuckTypedList: TDuckTypedList);
begin
  inherited Create;
  FDuckTypedList := ADuckTypedList;
  FPosition := -1;
end;

function TDormListEnumerator.DoGetCurrent: TObject;
begin
  if FPosition > -1 then begin
    Result := FDuckTypedList.GetItem(FPosition)
  end
  else begin
    raise Exception.Create('Enumerator error: Call MoveNext first');
  end;
end;

function TDormListEnumerator.DoMoveNext: Boolean;
begin
  if FPosition < FDuckTypedList.Count - 1 then begin
    Inc(FPosition);
    Result := True;
  end
  else begin
    Result := False;
  end;
end;

constructor TDuckTypedList.Create(AObjectAsDuck: TObject);
begin
  inherited Create;
  FObjectAsDuck := AObjectAsDuck;

  FAddMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Add');
  if not Assigned(FAddMethod) then begin
    raise EDormException.Create('Cannot find method "Add" in the duck object');
  end;

  FClearMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Clear');
  if not Assigned(FClearMethod) then begin
    raise EDormException.Create('Cannot find method "Clear" in the duck object');
  end;

  FGetItemMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetIndexedProperty('Items').ReadMethod;
  if not Assigned(FGetItemMethod) then begin
    FGetItemMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetItem');
  end;

  if not Assigned(FGetItemMethod) then begin
    FGetItemMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetElement');
  end;

  if not Assigned(FGetItemMethod) then begin
    raise EDormException.Create('"Items", "GetItem" and "GetElement" all missing in the duck object');
  end;

  FCountProperty := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetProperty('Count');
  if not Assigned(FCountProperty) then begin
    FGetCountMethod := FRttiContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Count');
    if not Assigned(FGetCountMethod) then begin
      raise EDormException.Create('Cannot find property/method "Count" in the duck object');
    end;
  end;
end;

destructor TDuckTypedList.Destroy;
begin
  inherited;
end;

procedure TDuckTypedList.Add(const AObj: TObject);
begin
  FAddMethod.Invoke(FObjectAsDuck, [AObj]);
end;

class function TDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean;
begin
  if not Assigned(AObjectAsDuck) then begin
    Result := False
  end
  else begin
    var LRttiTy := TRttiUtils.RttiContext.GetType(AObjectAsDuck.ClassInfo);
    Result := (LRttiTy.GetMethod('Add') <> nil)
      and (LRttiTy.GetMethod('Clear') <> nil)
      and (LRttiTy.GetIndexedProperty('Items').ReadMethod <> nil)
      and (LRttiTy.GetProperty('Count') <> nil)
      and ((LRttiTy.GetMethod('GetItem' ) <> nil) or (LRttiTy.GetMethod('GetElement') <> nil))
  end;
end;

procedure TDuckTypedList.Clear;
begin
  FClearMethod.Invoke(FObjectAsDuck, []);
end;

function TDuckTypedList.Count: Integer;
begin
  if Assigned(FCountProperty) then
    Result := FCountProperty.GetValue(FObjectAsDuck).AsInteger
  else
    Result := FGetCountMethod.Invoke(FObjectAsDuck, []).AsInteger;
end;

function TDuckTypedList.GetEnumerator: TDormListEnumerator;
begin
  Result := TDormListEnumerator.Create(Self);
end;

function TDuckTypedList.GetItem(const AItemIndex: Integer): TObject;
begin
  Result := FGetItemMethod.Invoke(FObjectAsDuck, [AItemIndex]).AsObject;
end;

function TDuckTypedList.get_OwnsObjects: Boolean;
begin
  Result := TRttiUtils.GetProperty(FObjectAsDuck, 'OwnsObjects').AsBoolean
end;

procedure TDuckTypedList.QuickSort(AList: IWrappedList; L, R: Integer; AComparer: TFunc<TObject, TObject, Integer>);
var
  I, J: Integer;
  LObj: TObject;
begin
  repeat
    I := L;
    J := R;
    LObj := AList.GetItem((L+R) shr 1);

    repeat
      while AComparer(TObject(AList.GetItem(I)), LObj) < 0 do Inc(I);
      while AComparer(TObject(AList.GetItem(J)), LObj) > 0 do Dec(J);

      if I <= J then begin
        TRttiUtils.MethodCall(AList.WrappedObject, 'Exchange', [I, J]);
        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then QuickSort(AList, L, J, AComparer);
    L := I;
  until I >= R;
end;

procedure TDuckTypedList.QuickSort(AList: IWrappedList; AComparer: TFunc<TObject, TObject, Integer>);
begin
  QuickSort(AList, 0, AList.Count - 1, AComparer);
end;

procedure TDuckTypedList.set_OwnsObjects(const Value: Boolean);
begin
  TRttiUtils.SetProperty(FObjectAsDuck, 'OwnsObjects', Value);
end;

procedure TDuckTypedList.Sort(const APropName: string; ADirection: TSortingDirection = Ascending);
begin
  if ADirection = Ascending then begin
    QuickSort(
      Self
      ,
      function(L, R: TObject): Integer
      begin
        Result := CompareValue(TRttiUtils.GetProperty(L, APropName), TRttiUtils.GetProperty(R, APropName));
      end
    )
  end
  else begin
    QuickSort(
      Self
      ,
      function(L, R: TObject): Integer
      begin
        Result := -1 * CompareValue(TRttiUtils.GetProperty(L, APropName), TRttiUtils.GetProperty(R, APropName));
      end
    );
  end;
end;

function TDuckTypedList.WrappedObject: TObject;
begin
  Result := FObjectAsDuck;
end;

end.
