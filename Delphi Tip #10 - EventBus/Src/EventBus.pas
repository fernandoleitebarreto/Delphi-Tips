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

unit EventBus;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TThreadMode = (
    Posting,
    Main,
    Async,
    Background
  );

  // Event memory management
  TEventMM = (
    Automatic,
    Manual,
    ManualAndFreeMain
  );

  TCloneEventCallback = function(const AObject: TObject): TObject of object;
  TCloneEventMethod = TFunc<TObject, TObject>;

  IEventBus = Interface
    ['{7BDF4536-F2BA-4FBA-B186-09E1EE6C7E35}']
    procedure AddCustomClassCloning(const AQualifiedClassName: string; const ACloneMethod: TCloneEventMethod);
    function IsRegisteredForChannels(ASubscriber: TObject): Boolean;
    function IsRegisteredForEvents(ASubscriber: TObject): Boolean;
    procedure Post(const AChannel: string; const aMessage: string); overload;
    procedure Post(AEvent: TObject; const AContext: string = ''; AEventMM: TEventMM = ManualAndFreeMain); overload;
    procedure RegisterSubscriberForChannels(ASubscriber: TObject);
    procedure RegisterSubscriberForEvents(ASubscriber: TObject);
    procedure RemoveCustomClassCloning(const AQualifiedClassName: string);
    procedure UnregisterForChannels(ASubscriber: TObject);
    procedure UnregisterForEvents(ASubscriber: TObject);
    procedure set_OnCloneEvent(const aCloneEvent: TCloneEventCallback);
    property OnCloneEvent: TCloneEventCallback write set_OnCloneEvent;
  end;

  SubscribeAttribute = class(TCustomAttribute)
  private
    FContext: string;
    FThreadMode: TThreadMode;
  public
    constructor Create(AThreadMode: TThreadMode = TThreadMode.Posting; const AContext: string = '');
    property Context: string read FContext;
    property ThreadMode: TThreadMode read FThreadMode;
  end;

  ChannelAttribute = class(TCustomAttribute)
  private
    FChannel: string;
    FThreadMode: TThreadMode;
  public
    constructor Create(const AChannel: string; AThreadMode: TThreadMode = TThreadMode.Posting);
    property Channel: string read FChannel;
    property ThreadMode: TThreadMode read FThreadMode;
  end;

  TBaseEventBusEvent<T> = class(TObject)
  private
    FData: T;
    FDataOwner: Boolean;
    procedure set_Data(const AValue: T);
    procedure set_DataOwner(const AValue: Boolean);
  public
    constructor Create; overload;
    constructor Create(AData: T); overload;
    destructor Destroy; override;
    property Data: T read FData write set_Data;
    property DataOwner: Boolean read FDataOwner write set_DataOwner;
  end;

function GlobalEventBus: IEventBus;

implementation

uses
  System.Rtti,
  EventBus.Core;

type
  TGlobalEventBus = class
  strict private
    class var FEventBus: IEventBus;
  public
    class constructor Create;
    class property EventBus: IEventBus read FEventBus;
  end;

constructor SubscribeAttribute.Create(AThreadMode: TThreadMode = TThreadMode.Posting; const AContext: string = '');
begin
  inherited Create;
  FContext := AContext;
  FThreadMode := AThreadMode;
end;

constructor ChannelAttribute.Create(const AChannel: string; AThreadMode: TThreadMode = TThreadMode.Posting);
const
  _DefaultChannel = 'Default';
begin
  if aChannel.IsEmpty then FChannel := _DefaultChannel else FChannel := AChannel;
  FThreadMode := AThreadMode;
end;

function GlobalEventBus: IEventBus;
begin
  Result := TGlobalEventBus.EventBus;
end;

class constructor TGlobalEventBus.Create;
begin
  FEventBus := TEventBus.Create;
end;

constructor TBaseEventBusEvent<T>.Create;
begin
  inherited Create;
end;

constructor TBaseEventBusEvent<T>.Create(AData: T);
begin
  inherited Create;
  DataOwner := True;
  Data := AData;
end;

destructor TBaseEventBusEvent<T>.Destroy;
var
  LValue: TValue;
begin
  LValue := TValue.From<T>(Data);
  if LValue.IsObject and DataOwner then LValue.AsObject.Free;
  inherited;
end;

procedure TBaseEventBusEvent<T>.set_Data(const AValue: T);
begin
  FData := AValue;
end;

procedure TBaseEventBusEvent<T>.set_DataOwner(const AValue: Boolean);
begin
  FDataOwner := AValue;
end;

end.
