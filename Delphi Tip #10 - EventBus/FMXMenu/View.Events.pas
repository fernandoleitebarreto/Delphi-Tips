unit View.Events;

interface

Type

  TEventMenuButton = class(TObject)
  private
    FButtonClickName: String;
    procedure SetButtonClickName(const Value: String);
    public
      property ButtonClickName : String read FButtonClickName write SetButtonClickName;
  end;

implementation

{ TEventMenuButton }

procedure TEventMenuButton.SetButtonClickName(const Value: String);
begin
  FButtonClickName := Value;
end;

end.
