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

unit EventBus.Utils.Rtti;

interface

uses
  Data.DB,
  Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  TRttiUtils = class sealed
  public
    class var RttiContext: TRttiContext;

    class function BuildClass(
      AQualifiedName: string;
      AParams: array of TValue
    ): TObject;

    class function Clone(
      ASrcObj: TObject
    ): TObject; static;

    class procedure CopyObject(ASrcObj: TObject; var ADesObj: TObject); static;

    class function CreateObject(
      AQualifiedClassName: string
    ): TObject; overload; static;

    class function CreateObject(
      ARttiType: TRttiType
    ): TObject; overload; static;

    class procedure DataSetToObject(
      ADataSet: TDataSet;
      AObj: TObject
    );

    class function EqualValues(
      ASrc: TValue;
      ADest: TValue
    ): Boolean;

    class function ExistsProperty(
      AObj: TObject;
      const APropName: string;
      out AProp: TRttiProperty
    ): Boolean;

    class function FindByProperty<T: class>(
      AList: TObjectList<T>;
      APropName: string;
      APropValue: TValue
    ): T;

    class function FindType(
      AQualifiedName: string
    ): TRttiType;

    class procedure ForEachProperty(
      AClass: TClass;
      AProc: TProc<TRttiProperty>
    );

    class function GetAttribute<T: TCustomAttribute>(
      const ARttiObj: TRttiObject
    ): T; overload;

    class function GetAttribute<T: TCustomAttribute>(
      const ARttiType: TRttiType
    ): T; overload;

    class function GetField(
      AObj: TObject;
      const APropName: string
    ): TValue; overload;

    class function GetFieldType(
      AProp: TRttiProperty
    ): string;

    class function GetGUID<T>: TGUID;

    class function GetMethod(
      AObj: TObject;
      AMethodName: string
    ): TRttiMethod;

    class function GetProperty(
      AObj: TObject;
      const APropName: string
    ): TValue;

    class function GetPropertyAsString(
      AObj: TObject;
      const APropName: string
    ): string; overload;

    class function GetPropertyAsString(
      AObj: TObject;
      AProp: TRttiProperty
    ): string; overload;

    class function GetPropertyType(
      AObj: TObject;
      APropName: string
    ): string;

    class function HasAttribute<T: class>(
      AObj: TObject;
      out AAttribute: T
      ): Boolean; overload;

    class function HasAttribute<T: class>(
      ARttiMember: TRttiMember;
      out AAttribute: T
    ): Boolean; overload;

    class function HasAttribute<T: class>(
      ARttiMember: TRttiType;
      out AAttribute: T
    ): Boolean; overload;

    class function HasAttribute<T: TCustomAttribute>(
      const AObj: TRttiObject
    ): Boolean; overload;

    class function HasAttribute<T: TCustomAttribute>(
      const AObj: TRttiObject;
      out AAttribute: T
    ): Boolean; overload;

    class function HasStringValueAttribute(
      ARttiMember: TRttiMember;
      out Value: string
    ): Boolean;

    class function MethodCall(
      AObj: TObject;
      AMethodName: string;
      AParams: array of TValue;
      AExceptionOnNotFound: Boolean = True
    ): TValue;

    class procedure ObjectToDataSet(
      AObj: TObject;
      AField: TField;
      var Value: Variant
    );

    class procedure SetField(
      AObj: TObject;
      const APropName: string;
      const Value: TValue
    ); overload;

    class procedure SetProperty(
      AObj: TObject;
      const APropName: string;
      const Value: TValue
    ); overload; static;

    class function ValueAsString(
      const Value: TValue;
      const APropType: string;
      const ACustomFormat: string
    ): string;
  end;

  StringValueAttribute = class abstract(TCustomAttribute)
  private
    FValue: string;
    procedure set_Value(const Value: string);
  public
    constructor Create(Value: string);
    property Value: string read FValue write set_Value;
  end;

function FieldFor(const APropName: string): string; inline;

implementation

uses
  System.Classes,
  System.TypInfo,
  EventBus.Utils.DuckList;

class function TRttiUtils.BuildClass(AQualifiedName: string; AParams: array of TValue): TObject;
begin
  var T := FindType(AQualifiedName);
  var V := T.GetMethod('Create').Invoke(T.AsInstance.MetaclassType, AParams);
  Result := V.AsObject;
end;

class function TRttiUtils.Clone(ASrcObj: TObject): TObject;
begin
  Result := nil;
  CopyObject(ASrcObj, Result);
end;

procedure CopyAsStream(ASrc: TObject; var ADes: TObject);
begin
  var LSrcStream := TStream(ASrc);
  if not Assigned(ADes) then ADes := TStream.Create;
  var LDesStream := TStream(ADes);
  var LSavedPosition := LSrcStream.Position;

  LSrcStream.Position := 0;
  LDesStream.Position := 0;
  LDesStream.CopyFrom(LSrcStream, LSrcStream.Size);
  LSrcStream.Position := LSavedPosition;
  LDesStream.Position := LSavedPosition;
end;

procedure CopyAsWrappedList(ASrc: TObject; var ADes: TObject);
begin
  if not Assigned(ADes) then begin
    ADes := TRttiUtils.CreateObject(TRttiUtils.RttiContext.GetType(ASrc.ClassType));
  end;

  var LSrcList := WrapAsList(ASrc);
  var LDesList := WrapAsList(ADes);

  LDesList.Clear;

  for var I := 0 to LSrcList.Count - 1 do begin
    var LNewObj := TRttiUtils.Clone(LSrcList.GetItem(I));
    LDesList.Add(LNewObj);
  end;
end;

class procedure TRttiUtils.CopyObject(ASrcObj: TObject; var ADesObj: TObject);
begin
  if not Assigned(ASrcObj) then begin
    FreeAndNil(ADesObj);
    Exit;
  end;

  var LRttiType := RttiContext.GetType(ASrcObj.ClassType);

  if not Assigned(ADesObj) then begin
    ADesObj := CreateObject(LRttiType);
  end;

  if ASrcObj.ClassType <> ADesObj.ClassType then begin
    raise Exception.Create('CopyObject source object and destination object are of different class type');
  end;

  for var LField in LRttiType.GetFields do begin
    if not LField.FieldType.IsInstance then begin
      LField.SetValue(ADesObj, LField.GetValue(ASrcObj))
    end
    else begin
      var LSrcFieldValAsObj := LField.GetValue(ASrcObj).AsObject;
      var LDesFieldValAsObj := LField.GetValue(ADesObj).AsObject; // Can be nil

      if LSrcFieldValAsObj is TStream then
        CopyAsStream(LSrcFieldValAsObj, LDesFieldValAsObj)
      else if TDuckTypedList.CanBeWrappedAsList(LSrcFieldValAsObj) then
        CopyAsWrappedList(LSrcFieldValAsObj, LDesFieldValAsObj)
      else
        TRttiUtils.CopyObject(LSrcFieldValAsObj, LDesFieldValAsObj);

      LField.SetValue(ADesObj, LDesFieldValAsObj);
    end
  end;
end;

class function TRttiUtils.CreateObject(AQualifiedClassName: string): TObject;
begin
  var LRttiType := RttiContext.FindType(AQualifiedClassName);
  if Assigned(LRttitype) then
    Result := CreateObject(LRttiType)
  else
    raise Exception.CreateFmt('Cannot find RTTI for %s. Is the type linked in the module?', [AQualifiedClassName]);
end;

class function TRttiUtils.CreateObject(ARttiType: TRttiType): TObject;
begin
  { First solution, clear and slow }
  Result := nil;
  for var LMethod in ARttiType.GetMethods do begin
    if LMethod.HasExtendedInfo and LMethod.IsConstructor then begin
      if Length(LMethod.GetParameters) = 0 then begin
        var LMetaClass := ARttiType.AsInstance.MetaclassType;
        Result := LMethod.Invoke(LMetaClass, []).AsObject;
        Break;
      end;
    end;
  end;

  if not Assigned(Result) then begin
    raise Exception.Create('Cannot find a proper constructor for ' + ARttiType.ToString);
  end;
  { Second solution, dirty and fast }
  // Result := TObject(ARttiType.GetMethod('Create').Invoke(ARttiType.AsInstance.MetaclassType, []).AsObject);
end;

class procedure TRttiUtils.DataSetToObject(ADataSet: TDataset; AObj: TObject);
begin
  var LRttiType := RttiContext.GetType(AObj.ClassType);
  var LProps := LRttiType.GetProperties;

  for var LProp in LProps do begin
    if not SameText(LProp.Name, 'ID') then begin
      var LField := ADataSet.FindField(LProp.Name);

      if Assigned(LField) and not LField.ReadOnly then begin
        if LField is TIntegerField then
          SetProperty(AObj, LProp.Name, TIntegerField(LField).Value)
        else
          SetProperty(AObj, LProp.Name, TValue.From<Variant>(LField.Value));
      end;
    end;
  end;
end;

class function TRttiUtils.EqualValues(ASrc, ADest: TValue): Boolean;
begin
  // Really UniCodeCompareStr (Annoying VCL Name for backwards compatablity)
  Result := AnsiCompareStr(ASrc.ToString, ADest.ToString) = 0;
end;

class function TRttiUtils.ExistsProperty(AObj: TObject; const APropName: string; out AProp: TRttiProperty): Boolean;
begin
  AProp := RttiContext.GetType(AObj.ClassInfo).GetProperty(APropName);
  Result := Assigned(AProp);
end;

class function TRttiUtils.FindByProperty<T>(AList: TObjectList<T>; APropName: string; APropValue: TValue): T;
begin
  Result := nil;
  var LFound := False;

  for var LElem in AList do begin
    var LVal := GetProperty(LElem, APropName);

    case LVal.Kind of
      tkInteger:
        LFound := LVal.AsInteger = APropValue.AsInteger;
      tkFloat:
        LFound := Abs(LVal.AsExtended - APropValue.AsExtended) < 0.001;
      tkString, tkLString, tkWString, tkUString:
        LFound := LVal.AsString = APropValue.AsString;
      tkInt64:
        LFound := LVal.AsInt64 = APropValue.AsInt64;
    else
      raise Exception.Create('Property type not supported');
    end;

    if LFound then Result := LElem;
  end;
end;

class function TRttiUtils.FindType(AQualifiedName: string): TRttiType;
begin
  Result := RttiContext.FindType(AQualifiedName);
end;

class procedure TRttiUtils.ForEachProperty(AClass: TClass; AProc: TProc<TRttiProperty>);
begin
  var LRttiTy := RttiContext.GetType(AClass);
  if Assigned(LRttiTy) then begin
    var LProps := LRttiTy.GetProperties;
    for var LProp in LProps do AProc(LProp);
  end;
end;

class function TRttiUtils.GetAttribute<T>(const ARttiObj: TRttiObject): T;
begin
  Result := nil;
  var LAttrs := ARttiObj.GetAttributes;
  for var LAttr in  LAttrs do begin
    if LAttr.ClassType.InheritsFrom(T) then Exit(T(LAttr));
  end;
end;

class function TRttiUtils.GetAttribute<T>(const ARttiType: TRttiType): T;
begin
  Result := nil;
  var LAttrs := ARttiType.GetAttributes;
  for var LAttr in LAttrs do
  begin
    if LAttr.ClassType.InheritsFrom(T) then Exit(T(LAttr));
  end;
end;

class function TRttiUtils.GetField(AObj: TObject; const APropName: string): TValue;
begin
  var LRttiTy := RttiContext.GetType(AObj.ClassType);
  if not Assigned(LRttiTy) then begin
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [LRttiTy.ToString]);
  end;

  var LField := LRttiTy.GetField(FieldFor(APropName));
  if Assigned(LField) then begin
    Result := LField.GetValue(AObj)
  end
  else begin
    var LProp := LRttiTy.GetProperty(APropName);
    if not Assigned(LProp) then begin
      raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [LRttiTy.ToString, APropName]);
    end;
    Result := LProp.GetValue(AObj);
  end;
end;

class function TRttiUtils.GetFieldType(AProp: TRttiProperty): string;
begin
  var LPropTyInfo: PTypeInfo := AProp.PropertyType.Handle;

  if LPropTyInfo.Kind in [tkString, tkWString, tkChar, tkWChar, tkLString, tkUString] then
    Result := 'string'
  else if LPropTyInfo.Kind in [tkInteger, tkInt64] then
    Result := 'integer'
  else if LPropTyInfo = TypeInfo(TDate) then
    Result := 'date'
  else if LPropTyInfo = TypeInfo(TDateTime) then
    Result := 'datetime'
  else if LPropTyInfo = TypeInfo(Currency) then
    Result := 'decimal'
  else if LPropTyInfo = TypeInfo(TTime) then
    Result := 'time'
  else if LPropTyInfo.Kind = tkFloat then
    Result := 'float'
  else if (LPropTyInfo.Kind = tkEnumeration) { and (LPropTyInfo.Name = 'Boolean') } then
    Result := 'Boolean'
  else if AProp.PropertyType.IsInstance and AProp.PropertyType.AsInstance.MetaclassType.InheritsFrom(TStream) then
    Result := 'blob'
  else
    Result := EmptyStr;
end;

class function TRttiUtils.GetGUID<T>: TGUID;
begin
  var LRttiTy := RttiContext.GetType(TypeInfo(T));
  if not (LRttiTy.TypeKind = tkInterface) then raise Exception.Create('Type is no interface');
  Result := TRttiInterfaceType(LRttiTy).GUID;
end;

class function TRttiUtils.GetMethod(AObj: TObject; AMethodName: string): TRttiMethod;
begin
  var LRttiTy := RttiContext.GetType(AObj.ClassInfo);
  Result := LRttiTy.GetMethod(AMethodName);
end;

class function TRttiUtils.GetProperty(AObj: TObject; const APropName: string): TValue;
begin
  var LRttiType := RttiContext.GetType(AObj.ClassType);
  if not Assigned(LRttiType) then begin
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [LRttiType.ToString]);
  end;

  var LProp := LRttiType.GetProperty(APropName);
  if not Assigned(LProp) then begin
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [LRttiType.ToString, APropName]);
  end;

  if LProp.IsReadable then
    Result := LProp.GetValue(AObj)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]', [LRttiType.ToString, APropName]);
end;

class function TRttiUtils.GetPropertyAsString(AObj: TObject; const APropName: string): string;
begin
  var LProp := RttiContext.GetType(AObj.ClassType).GetProperty(APropName);
  if Assigned(LProp) then begin
    Result := GetPropertyAsString(AObj, LProp)
  end
  else begin
    Result := ''
  end;
end;

class function TRttiUtils.GetPropertyAsString(AObj: TObject; AProp: TRttiProperty): string;
begin
  if AProp.IsReadable then begin
    var LVal := AProp.GetValue(AObj);
    var LFieldTy := GetFieldType(AProp);
    var LCustomFormat: string;
    HasStringValueAttribute(AProp, LCustomFormat);
    Result := ValueAsString(LVal, LFieldTy, LCustomFormat);
  end
  else begin
    Result := '';
  end;
end;

class function TRttiUtils.GetPropertyType(AObj: TObject; APropName: string): string;
begin
  Result := GetFieldType(RttiContext.GetType(AObj.ClassInfo).GetProperty(APropName));
end;

{ TListDuckTyping }
class function TRttiUtils.HasAttribute<T>(AObj: TObject; out AAttribute: T): Boolean;
begin
  Result := HasAttribute<T>(RttiContext.GetType(AObj.ClassType), AAttribute)
end;

class function TRttiUtils.HasAttribute<T>(ARttiMember: TRttiMember; out AAttribute: T): Boolean;
begin
  AAttribute := nil;
  Result := False;
  var LAttrs := ARttiMember.GetAttributes;
  for var LAttr in LAttrs do begin
    if LAttr is T then begin
      AAttribute := T(LAttr);
      Exit(True);
    end;
  end;
end;

class function TRttiUtils.HasAttribute<T>(const AObj: TRttiObject): Boolean;
begin
  Result := Assigned(GetAttribute<T>(AObj));
end;

class function TRttiUtils.HasAttribute<T>(const AObj: TRttiObject; out AAttribute: T): Boolean;
begin
  AAttribute := GetAttribute<T>(AObj);
  Result := Assigned(AAttribute);
end;

class function TRttiUtils.HasAttribute<T>(ARttiMember: TRttiType; out AAttribute: T): Boolean;
begin
  AAttribute := nil;
  Result := False;
  var LAttrs := ARttiMember.GetAttributes;
  for var LAttr in LAttrs do begin
    if LAttr is T then begin
      AAttribute := T(LAttr);
      Exit(True);
    end;
  end;
end;

class function TRttiUtils.HasStringValueAttribute(ARttiMember: TRttiMember; out Value: string): Boolean;
var
  LAttr: StringValueAttribute;
begin
  Result := HasAttribute<StringValueAttribute>(ARttiMember, LAttr);

  if Result then begin
    Value := StringValueAttribute(LAttr).Value
  end
  else begin
    Value := '';
  end;
end;

class function TRttiUtils.MethodCall(AObj: TObject; AMethodName: string; AParams: array of TValue;
  AExceptionOnNotFound: Boolean): TValue;
begin
  Result := nil;
  var LFound := False;
  var LRttiType := RttiContext.GetType(AObj.ClassInfo);
  var LParamsLen := Length(AParams);

  for var LRttiMethod in LRttiType.GetMethods do begin
    var LMethodParamsLen := Length(LRttiMethod.GetParameters);
    if LRttiMethod.Name.Equals(AMethodName) and (LMethodParamsLen = LParamsLen) then begin
      LFound := True;
      Result := LRttiMethod.Invoke(AObj, AParams);
      Break;
    end;
  end;

  if not LFound and AExceptionOnNotFound then begin
    raise Exception.CreateFmt('Cannot find compatible mehod "%s" in the object', [AMethodName]);
  end;
end;

class procedure TRttiUtils.ObjectToDataSet(AObj: TObject; AField: TField; var Value: Variant);
begin
  Value := GetProperty(AObj, AField.FieldName).AsVariant;
end;

class procedure TRttiUtils.SetField(AObj: TObject; const APropName: string; const Value: TValue);
begin
  var LRttiType := RttiContext.GetType(AObj.ClassType);
  if not Assigned(LRttiType) then raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [LRttiType.ToString]);

  var LField := LRttiType.GetField(FieldFor(APropName));
  if Assigned(LField) then begin
    LField.SetValue(AObj, Value)
  end
  else begin
    var LProp := LRttiType.GetProperty(APropName);
    if Assigned(LProp) then
      if LProp.IsWritable then LProp.SetValue(AObj, Value)
    else
      raise Exception.CreateFmt('Cannot get RTTI for field or property [%s.%s]', [LRttiType.ToString, APropName]);
  end;
end;

class procedure TRttiUtils.SetProperty(AObj: TObject; const APropName: string; const Value: TValue);
begin
  var LRttiType := RttiContext.GetType(AObj.ClassType);
  if not Assigned(LRttiType) then begin
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [LRttiType.ToString]);
  end;

  var LProp := LRttiType.GetProperty(APropName);
  if not Assigned(LProp) then begin
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [LRttiType.ToString, APropName]);
  end;

  if LProp.IsWritable then
    LProp.SetValue(AObj, Value)
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]', [LRttiType.ToString, APropName]);  
end;

class function TRttiUtils.ValueAsString(const Value: TValue; const APropType, ACustomFormat: string): string;
begin
  case Value.Kind of
    tkUnknown:
      Result := '';
    tkInteger:
      Result := IntToStr(Value.AsInteger);
    tkChar:
      Result := Value.AsString;
    tkEnumeration:
      if APropType = 'Boolean' then
        Result := BoolToStr(Value.AsBoolean, True)
      else
        Result := '(enumeration)';
    tkFloat:
      begin
        if APropType = 'datetime' then
          if ACustomFormat = '' then
            Result := DateTimeToStr(Value.AsExtended)
          else
            Result := FormatDateTime(ACustomFormat, Value.AsExtended)
        else if APropType = 'date' then
          if ACustomFormat = '' then
            Result := DateToStr(Value.AsExtended)
          else
            Result := FormatDateTime(ACustomFormat, Trunc(Value.AsExtended))
        else if APropType = 'time' then
          if ACustomFormat = '' then
            Result := TimeToStr(Value.AsExtended)
          else
            Result := FormatDateTime(ACustomFormat, Frac(Value.AsExtended))
        else
          if ACustomFormat = '' then
            Result := FloatToStr(Value.AsExtended)
          else
            Result := FormatFloat(ACustomFormat, Value.AsExtended);
      end;
    tkString:
      Result := Value.AsString;
    tkSet:
      Result := '(set)';
    tkClass:
      Result := Value.AsObject.QualifiedClassName;
    tkMethod:
      Result := '(method)';
    tkWChar:
      Result := Value.AsString;
    tkLString:
      Result := Value.AsString;
    tkWString:
      Result := Value.AsString;
    tkVariant:
      Result := string(Value.AsVariant);
    tkArray:
      Result := '(array)';
    tkRecord:
      Result := '(record)';
    tkInterface:
      Result := '(interface)';
    tkInt64:
      Result := IntToStr(Value.AsInt64);
    tkDynArray:
      Result := '(array)';
    tkUString:
      Result := Value.AsString;
    tkClassRef:
      Result := '(classref)';
    tkPointer:
      Result := '(pointer)';
    tkProcedure:
      Result := '(procedure)';
  end;
end;

function FieldFor(const APropName: string): string; inline;
begin
  Result := 'F' + APropName;
end;

constructor StringValueAttribute.Create(Value: string);
begin
  inherited Create;
  FValue := Value;
end;

procedure StringValueAttribute.set_Value(const Value: string);
begin
  FValue := Value;
end;

end.
