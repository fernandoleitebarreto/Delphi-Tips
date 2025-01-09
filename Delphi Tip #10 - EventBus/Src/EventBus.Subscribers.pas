{ *******************************************************************************
  Copyright 2016-2020 Daniele Spinetti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  Updated by Wuping Xin Copyright (c) 2020
  ******************************************************************************** }

unit EventBus.Subscribers;

interface

uses
  System.Rtti,
  EventBus;

type
  TSubscriberMethod = class(TObject)
  private
    FContext: string;
    FEventType: TClass;
    FMethod: TRttiMethod;
    FThreadMode: TThreadMode;
  public
    constructor Create(ARttiMethod: TRttiMethod; AEventType: TClass; AThreadMode: TThreadMode; const AContext: string = '');
    function Equals(AObj: TObject): Boolean; override;

    property Context: string read FContext;
    property EventType: TClass read FEventType;
    property Method: TRttiMethod read FMethod;
    property ThreadMode: TThreadMode read FThreadMode;
  end;

  TSubscription = class(TObject)
  private
    FActive: Boolean;
    FSubscriber: TObject;
    FSubscriberMethod: TSubscriberMethod;
    function get_Active: Boolean;
    procedure set_Active(const Value: Boolean);
    function get_Context: string;
  public
    constructor Create(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
    destructor Destroy; override;
    function Equals(AObj: TObject): Boolean; override;

    property Active: Boolean read get_Active write set_Active;
    property Context: string read get_Context;
    property Subscriber: TObject read FSubscriber;
    property SubscriberMethod: TSubscriberMethod read FSubscriberMethod;
  end;

  TSubscribersFinder = class(TObject)
  public
    class function FindChannelsSubcriberMethods(
      ASubscriberClass: TClass;
      AExceptionOnEmpty: Boolean = False
    ): TArray<TSubscriberMethod>;

    class function FindEventsSubscriberMethods(
      ASubscriberClass: TClass;
      AExceptionOnEmpty: Boolean = False
    ): TArray<TSubscriberMethod>;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  EventBus.Utils.Rtti;

constructor TSubscriberMethod.Create(ARttiMethod: TRttiMethod; AEventType: TClass;
  AThreadMode: TThreadMode; const AContext: string = '');
begin
  FMethod := ARttiMethod;
  FEventType := AEventType;
  FThreadMode := AThreadMode;
  FContext := AContext;
end;

function TSubscriberMethod.Equals(AObj: TObject): Boolean;
begin
  Result := AObj is TSubscriberMethod;
  if Result then begin
    var LOther := TSubscriberMethod(AObj);
    Result := inherited Equals(LOther) or (LOther.Method.Tostring = Method.Tostring);
  end;
end;

class function TSubscribersFinder.FindChannelsSubcriberMethods(ASubscriberClass: TClass;
  AExceptionOnEmpty: Boolean = False): TArray<TSubscriberMethod>;
begin
  Result := [];
  var LRttiType := TRttiUtils.RttiContext.GetType(ASubscriberClass);
  var LRttiMethods := LRttiType.GetMethods;
  var LChannelAttr: ChannelAttribute;

  for var LRttiMethod in LRttiMethods do begin
    if TRttiUtils.HasAttribute<ChannelAttribute>(LRttiMethod, LChannelAttr) then begin
      // Check number of parameters of the method. A "Channel" method can only have single argument of string type.
      var LParamsLen := Length(LRttiMethod.GetParameters);
      if ((LParamsLen <> 1) or (LRttiMethod.GetParameters[0].ParamType.TypeKind <> tkUstring)) then begin
        raise Exception.CreateFmt(
          'Method  %s has Channel attribute with %d arguments. Only a single argument of string type allowed.',
          [LRttiMethod.Name, LParamsLen]
        );
      end;
      // Create an instance of SubscriberMethod.
      var LSubscriberMethod := TSubscriberMethod.Create(
        LRttiMethod,
        nil,
        LChannelAttr.ThreadMode,
        LChannelAttr.Channel // The method's Context property is Channel (name).
      );
      // Add the SubscriberMethod to the array.
      Result := Result + [LSubscriberMethod];
    end;
  end;

  if (Length(Result) < 1) and AExceptionOnEmpty then begin
    raise Exception.CreateFmt(
      'The class %s and its super classes have no public methods with Channel attribute.',
      [ASubscriberClass.QualifiedClassName]
    );
  end;
end;

class function TSubscribersFinder.FindEventsSubscriberMethods(ASubscriberClass: TClass;
  AExceptionOnEmpty: Boolean = False): TArray<TSubscriberMethod>;
begin
  Result := [];
  var LRttiTy := TRttiUtils.RttiContext.GetType(ASubscriberClass);
  var LRttiMethods := LRttiTy.GetMethods;
  var LSubscribeAttr: SubscribeAttribute;

  for var LRttiMethod in LRttiMethods do begin
    if TRttiUtils.HasAttribute<SubscribeAttribute>(LRttiMethod, LSubscribeAttr) then begin
      var LParamsLen := Length(LRttiMethod.GetParameters);
      if (LParamsLen <> 1) then begin
        raise Exception.CreateFmt(
          'Method  %s has Subscribe attribute requiring %d arguments. Only single argument allowed.',
          [LRttiMethod.Name, LParamsLen]
        );
      end;
      // Create an instance of SubscriberMethod with the EventType.
      var LEventType := LRttiMethod.GetParameters[0].ParamType.Handle.TypeData.ClassType;
      var LSubscriberMethod := TSubscriberMethod.Create(
        LRttiMethod,
        LEventType,
        LSubscribeAttr.ThreadMode,
        LSubscribeAttr.Context
      );
      // Add the SubscriberMethod to the array.
      Result := Result + [LSubscriberMethod];
    end;
  end;

  if (Length(Result) < 1) and AExceptionOnEmpty then begin
    raise Exception.CreateFmt(
      'The class %s and its super classes have no public methods with Subscribe attribute.',
      [ASubscriberClass.QualifiedClassName]
    );
  end;
end;

constructor TSubscription.Create(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
begin
  inherited Create;
  FActive := True;
  FSubscriber := ASubscriber;
  FSubscriberMethod := ASubscriberMethod;
end;

destructor TSubscription.Destroy;
begin
  FSubscriberMethod.Free;
  inherited;
end;

function TSubscription.Equals(AObj: TObject): Boolean;
begin
  Result := (AObj is TSubscription);
  if Result then begin
    var LOther := TSubscription(AObj);
    Result := (Subscriber = LOther.Subscriber) and (SubscriberMethod.Equals(LOther.SubscriberMethod));
  end;
end;

function TSubscription.get_Active: Boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FActive;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TSubscription.get_Context: string;
begin
  Result := SubscriberMethod.Context;
end;

procedure TSubscription.set_Active(const Value: Boolean);
begin
  TMonitor.Enter(Self);
  try
    FActive := Value;
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
