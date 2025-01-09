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

unit EventBus.Core;

interface

uses
  Generics.Collections,
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  EventBus,
  EventBus.Subscribers;

type
  TEventBus = class(TInterfacedObject, IEventBus)
  private
    class var MREW: TLightweightMREW;
  private
    FCustomClonerDict: TDictionary<string, TCloneEventMethod>;
    FOnCloneEvent: TCloneEventCallback;
    FSubscriberChannels: TObjectDictionary<TObject, TList<string>>;
    FSubscriberEvents: TObjectDictionary<TObject, TList<TClass>>;
    FSubscriptionsOfGivenChannel: TObjectDictionary<string, TObjectList<TSubscription>>;
    FSubscriptionsOfGivenEventType: TObjectDictionary<TClass, TObjectList<TSubscription>>;
    function GenerateThreadProc(ASubscription: TSubscription; AMessage: string): TThreadProcedure; overload;
    function GenerateThreadProc(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM): TThreadProcedure; overload;
    function GenerateTProc(ASubscription: TSubscription; AMessage: string): TProc; overload;
    function GenerateTProc(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM): TProc; overload;
    procedure InvokeSubscriber(ASubscription: TSubscription; AMessage: string); overload;
    procedure InvokeSubscriber(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM); overload;
    procedure SubscribeChannel(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
    procedure SubscribeEvent(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
    procedure UnsubscribeByChannel(ASubscriber: TObject; AChannel: string);
    procedure UnsubscribeByEventType(ASubscriber: TObject; AEventType: TClass);
    procedure set_OnCloneEvent(const ACloneEvent: TCloneEventCallback);
  protected
    function CloneEvent(AEvent: TObject): TObject; virtual;

    procedure PostToChannel(
      ASubscription: TSubscription;
      AMessage: string;
      AIsMainThread: Boolean
    ); virtual;

    procedure PostToSubscription(
      ASubscription: TSubscription;
      AEvent: TObject;
      AIsMainThread: Boolean;
      AEventMM: TEventMM
     ); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    {$REGION 'IEventBus Interface Methods'}
    procedure AddCustomClassCloning(const AQualifiedClassName: string; const ACloneMethod: TCloneEventMethod);
    function IsRegisteredForChannels(ASubscriber: TObject): Boolean;
    function IsRegisteredForEvents(ASubscriber: TObject): Boolean;
    procedure Post(const AChannel: string; const AMessage: string); overload; virtual;
    procedure Post(AEvent: TObject; const AContext: string = ''; AEventMM: TEventMM = ManualAndFreeMain); overload; virtual;
    procedure RegisterSubscriberForChannels(ASubscriber: TObject); virtual;
    procedure RegisterSubscriberForEvents(ASubscriber: TObject); virtual;
    procedure RemoveCustomClassCloning(const AQualifiedClassName: string);
    procedure UnregisterForChannels(ASubscriber: TObject); virtual;
    procedure UnregisterForEvents(ASubscriber: TObject); virtual;
    {$ENDREGION}

    property OnCloneEvent: TCloneEventCallback write set_OnCloneEvent;
  end;

implementation

uses
  System.Rtti,
  System.Threading,
  EventBus.Utils.Rtti;

constructor TEventBus.Create;
begin
  inherited Create;
  FCustomClonerDict := TDictionary<string, TCloneEventMethod>.Create;
  FSubscriberChannels := TObjectDictionary<TObject, TList<string>>.Create([doOwnsValues]);
  FSubscriberEvents := TObjectDictionary<TObject, TList<TClass>>.Create([doOwnsValues]);
  FSubscriptionsOfGivenChannel := TObjectDictionary<string, TObjectList<TSubscription>>.Create([doOwnsValues]);
  FSubscriptionsOfGivenEventType := TObjectDictionary<TClass, TObjectList<TSubscription>>.Create([doOwnsValues]);
end;

destructor TEventBus.Destroy;
begin
  FCustomClonerDict.Free;
  FSubscriberChannels.Free;
  FSubscriberEvents.Free;
  FSubscriptionsOfGivenChannel.Free;
  FSubscriptionsOfGivenEventType.Free;
  inherited;
end;

procedure TEventBus.AddCustomClassCloning(const AQualifiedClassName: string; const ACloneMethod: TCloneEventMethod);
begin
  FCustomClonerDict.Add(AQualifiedClassName, ACloneMethod);
end;

function TEventBus.CloneEvent(AEvent: TObject): TObject;
var
  LCloneMethod: TCloneEventMethod;
begin
  if FCustomClonerDict.TryGetValue(AEvent.QualifiedClassName, LCloneMethod) then
    Result := LCloneMethod(AEvent)
  else if Assigned(FOnCloneEvent) then
    Result := FOnCloneEvent(AEvent)
  else
    Result := TRttiUtils.Clone(AEvent);
end;

function TEventBus.GenerateThreadProc(ASubscription: TSubscription; AMessage: string): TThreadProcedure;
begin
  Result :=
    procedure
    begin
      if ASubscription.Active then begin
        ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AMessage]);
      end;
    end;
end;

function TEventBus.GenerateThreadProc(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM): TThreadProcedure;
begin
  Result :=
    procedure
    begin
      if ASubscription.Active then begin
        ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AEvent]);
      end;

      if (AEventMM = TEventMM.Automatic) then begin
        AEvent.Free;
      end;
    end;
end;

function TEventBus.GenerateTProc(ASubscription: TSubscription; AMessage: string): TProc;
begin
  Result :=
    procedure
    begin
      if ASubscription.Active then begin
        ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AMessage])
      end
    end;
end;

function TEventBus.GenerateTProc(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM): TProc;
begin
  Result :=
    procedure
    begin
      if ASubscription.Active then begin
        ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AEvent]);
      end;

      if (AEventMM = TEventMM.Automatic) then begin
        AEvent.Free;
      end;
    end;
end;

procedure TEventBus.InvokeSubscriber(ASubscription: TSubscription; AMessage: string);
begin
  try
    ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AMessage]);
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt(
        'Error invoking subscriber method. Subscriber class: %s. Channel: %s. Original exception: %s: %s', [
         ASubscription.Subscriber.ClassName,
         ASubscription.SubscriberMethod.Context,
         E.ClassName,
         E.Message
      ]);
    end;
  end;
end;

procedure TEventBus.InvokeSubscriber(ASubscription: TSubscription; AEvent: TObject; AEventMM: TEventMM);
begin
  try
    ASubscription.SubscriberMethod.Method.Invoke(ASubscription.Subscriber, [AEvent]);
    if (AEventMM = TEventMM.Automatic) then AEvent.Free;
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt(
        'Error invoking subscriber method. Subscriber class: %s. Event type: %s. Original exception: %s: %s', [
         ASubscription.Subscriber.ClassName,
         ASubscription.SubscriberMethod.EventType.ClassName,
         E.ClassName,
         E.Message
      ]);
    end;
  end;
end;

function TEventBus.IsRegisteredForChannels(ASubscriber: TObject): Boolean;
begin
  MREW.BeginRead;
  try
    Result := FSubscriberChannels.ContainsKey(ASubscriber);
  finally
    MREW.EndRead;
  end;
end;

function TEventBus.IsRegisteredForEvents(ASubscriber: TObject): Boolean;
begin
  MREW.BeginRead;
  try
    Result := FSubscriberEvents.ContainsKey(ASubscriber);
  finally
    MREW.EndRead;
  end;
end;

procedure TEventBus.Post(const AChannel, AMessage: string);
begin
  MREW.BeginRead;
  try
    var LSubscriptions: TObjectList<TSubscription>;
    if FSubscriptionsOfGivenChannel.TryGetValue(AChannel, LSubscriptions) then begin
      for var LSubscription in LSubscriptions do begin
        if LSubscription.Active and (LSubscription.Context = AChannel) then begin
          var LIsMainThread := (MainThreadID = TThread.CurrentThread.ThreadID);
          PostToChannel(LSubscription, AMessage, LIsMainThread);
        end;
      end
    end;
  finally
    MREW.EndRead;
  end;
end;

procedure TEventBus.Post(AEvent: TObject; const AContext: string; AEventMM: TEventMM);
begin
  Assert(Assigned(AEvent), 'Event cannot be nil'); // Main event must not be nil.

  MREW.BeginRead;
  try
    try
      var LSubscriptions: TObjectList<TSubscription>;
      if FSubscriptionsOfGivenEventType.TryGetValue(AEvent.ClassType, LSubscriptions) then begin
        for var LSubscription in LSubscriptions do begin
          if LSubscription.Active and (LSubscription.Context = AContext) then begin
            // Clone for each LSubscription.
            var LEvent := CloneEvent(AEvent);
            var LIsMainThread := (MainThreadID = TThread.CurrentThread.ThreadID);
            PostToSubscription(LSubscription, LEvent, LIsMainThread, AEventMM);
          end;
        end;
      end;
    finally
      if (AEventMM in [Automatic, ManualAndFreeMain]) then AEvent.Free; // Free "main" event.
    end;
  finally
    MREW.EndRead;
  end;
end;

procedure TEventBus.PostToChannel(ASubscription: TSubscription; AMessage: string; AIsMainThread: Boolean);
begin
  case ASubscription.SubscriberMethod.ThreadMode of
    Posting:
      InvokeSubscriber(ASubscription, AMessage);
    Main:
      if (AIsMainThread) then
        InvokeSubscriber(ASubscription, AMessage)
      else
        TThread.Queue(nil, GenerateThreadProc(ASubscription, AMessage));
    Background:
      if (AIsMainThread) then
        TTask.Run(GenerateTProc(ASubscription, AMessage))
      else
        InvokeSubscriber(ASubscription, AMessage);
    Async:
      TTask.Run(GenerateTProc(ASubscription, AMessage));
  else
    raise Exception.Create('Unknown thread mode');
  end;
end;

procedure TEventBus.PostToSubscription(ASubscription: TSubscription; AEvent: TObject;
  AIsMainThread: Boolean; AEventMM: TEventMM);
begin
  case ASubscription.SubscriberMethod.ThreadMode of
    Posting:
      InvokeSubscriber(ASubscription, AEvent, AEventMM);
    Main:
      if (AIsMainThread) then
        InvokeSubscriber(ASubscription, AEvent, AEventMM)
      else
        TThread.Queue(nil, GenerateThreadProc(ASubscription, AEvent, AEventMM));
    Background:
      if (AIsMainThread) then
        TTask.Run(GenerateTProc(ASubscription, AEvent, AEventMM))
      else
        InvokeSubscriber(ASubscription, AEvent, AEventMM);
    Async:
      TTask.Run(GenerateTProc(ASubscription, AEvent, AEventMM));
  else
    raise Exception.Create('Unknown thread mode');
  end;

end;

procedure TEventBus.RegisterSubscriberForChannels(ASubscriber: TObject);
begin
  MREW.BeginWrite;
  try
    if Assigned(ASubscriber) then begin
      var LMethods := TSubscribersFinder.FindChannelsSubcriberMethods(ASubscriber.ClassType, True);
      for var LMethod in LMethods do SubscribeChannel(ASubscriber, LMethod);
    end
    else begin
      raise Exception.Create('Invalid subscriber with null reference.');
    end;
  finally
   MREW.EndWrite;
  end;
end;

procedure TEventBus.RegisterSubscriberForEvents(ASubscriber: TObject);
begin
  MREW.BeginWrite;
  try
    if Assigned(ASubscriber) then begin
      var LMethods := TSubscribersFinder.FindEventsSubscriberMethods(ASubscriber.ClassType, True);
      for var LMethod in LMethods do SubscribeEvent(ASubscriber, LMethod);
    end
    else begin
      raise Exception.Create('Invalid subscriber with null reference.');
    end;
  finally
    MREW.EndWrite;
  end;
end;

procedure TEventBus.RemoveCustomClassCloning(const AQualifiedClassName: string);
begin
  // No exception is thrown if the key is not in the dictionary
  FCustomClonerDict.Remove(AQualifiedClassName);
end;

procedure TEventBus.set_OnCloneEvent(const ACloneEvent: TCloneEventCallback);
begin
  FOnCloneEvent := ACloneEvent;
end;

procedure TEventBus.SubscribeChannel(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
begin
  var LChannel := ASubscriberMethod.Context;

  // Check and create the Channel-Subscriptions map, if not existing.
  if not FSubscriptionsOfGivenChannel.ContainsKey(LChannel) then begin
    FSubscriptionsOfGivenChannel.Add(LChannel, TObjectList<TSubscription>.Create());
  end;

  // Check and create the Subscriber-Channels map, if not existing.
  if not FSubscriberChannels.ContainsKey(ASubscriber) then begin
    FSubscriberChannels.Add(ASubscriber, TList<string>.Create);
  end;

  // Check if the subscription has been registered already.
  var LSubscriptions := FSubscriptionsOfGivenChannel.Items[LChannel];
  for var LSubscription in LSubscriptions do begin
    if LSubscription.Subscriber.Equals(ASubscriber) and LSubscription.SubscriberMethod.Equals(ASubscriberMethod) then begin
      raise Exception.CreateFmt('Subscriber %s already registered to channel %s ', [ASubscriber.ClassName, LChannel]);
    end;
  end;

  // Add the new subscription to subscription-list of the given channel.
  var LNewSubscription := TSubscription.Create(ASubscriber, ASubscriberMethod);
  LSubscriptions.Add(LNewSubscription);

  // Add the new channel to the channel-list of the given subscriber.
  var LChannels := FSubscriberChannels[ASubscriber];
  if not LChannels.Contains(LChannel) then LChannels.Add(LChannel);
end;

procedure TEventBus.SubscribeEvent(ASubscriber: TObject; ASubscriberMethod: TSubscriberMethod);
begin
  var LEvent := ASubscriberMethod.EventType;

  // Check and create EventType-Subscriptions map, if not existing.
  if (not FSubscriptionsOfGivenEventType.ContainsKey(LEvent)) then begin
    FSubscriptionsOfGivenEventType.Add(LEvent, TObjectList<TSubscription>.Create);
  end;

  // Check and create Subscriber-EventTypes map, if not existing.
  if not FSubscriberEvents.ContainsKey(ASubscriber) then begin
    FSubscriberEvents.Add(ASubscriber, TList<TClass>.Create);
  end;

  // Check if the subscription has been registered already.
  var LSubscriptions := FSubscriptionsOfGivenEventType.Items[LEvent];
  for var LSubscription in LSubscriptions do begin
    if LSubscription.Subscriber.Equals(ASubscriber) and LSubscription.SubscriberMethod.Equals(ASubscriberMethod) then begin
      raise Exception.CreateFmt('Subscriber %s already registered to event %s ', [ASubscriber.ClassName, LEvent.ClassName]);
    end;
  end;

  // Add the new subscription the subscription-list of the given type.
  var LNewSubscription := TSubscription.Create(ASubscriber, ASubscriberMethod);
  LSubscriptions.Add(LNewSubscription); // Subscription instances life time is auto-managed by its owner list.

  // Add the new event to the event-list of the given subscriber.
  var LEvents := FSubscriberEvents[ASubscriber];
  if not LEvents.Contains(LEvent) then LEvents.Add(LEvent); // Add only if not existing.
end;

procedure TEventBus.UnregisterForChannels(ASubscriber: TObject);
begin
  MREW.BeginWrite;
  try
    var LChannels: TList<string>;
    if FSubscriberChannels.TryGetValue(ASubscriber, LChannels) then begin
      for var LChannel in LChannels do UnsubscribeByChannel(ASubscriber, LChannel);
      FSubscriberChannels.Remove(ASubscriber);
    end;
  finally
    MREW.EndWrite;
  end;
end;

procedure TEventBus.UnregisterForEvents(ASubscriber: TObject);
begin
  MREW.BeginWrite;
  try
    var LEvents: TList<TClass>;
    if FSubscriberEvents.TryGetValue(ASubscriber, LEvents) then
    begin
      for var LEventType in LEvents do UnsubscribeByEventType(ASubscriber, LEventType);
      FSubscriberEvents.Remove(ASubscriber);
    end;
  finally
    MREW.EndWrite;
  end;
end;

procedure TEventBus.UnsubscribeByChannel(ASubscriber: TObject; AChannel: string);
begin
  var LSubscriptions := FSubscriptionsOfGivenChannel.Items[AChannel];
  if Assigned(LSubscriptions) and (LSubscriptions.Count > 0) then begin
    for var I := LSubscriptions.Count - 1 downto 0 do begin
      if LSubscriptions[I].Subscriber = ASubscriber then begin
        LSubscriptions[I].Active := False;
        LSubscriptions.Delete(I);
      end;
    end;
  end;
end;

procedure TEventBus.UnsubscribeByEventType(ASubscriber: TObject; AEventType: TClass);
begin
  var LSubscriptions := FSubscriptionsOfGivenEventType.Items[AEventType];
  if Assigned(LSubscriptions) and (LSubscriptions.Count > 0) then begin
    for var I := LSubscriptions.Count - 1 downto 0 do begin
      if LSubscriptions[I].Subscriber = ASubscriber then begin
        LSubscriptions[I].Active := False;
        LSubscriptions.Delete(I);
      end;
    end;
  end;
end;

end.
