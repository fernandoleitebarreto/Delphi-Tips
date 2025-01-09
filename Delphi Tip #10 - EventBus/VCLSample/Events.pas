unit Events;

interface

type
  TEventMemoChange = class(TObject)
  private
    FText: String;
    procedure SetText(const Value: String);
    public
      property Text : String read FText write SetText;
  end;

  TEventCheckBox = class(TObject)
  private
    FChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    public
      property Checked : Boolean read FChecked write SetChecked;
  end;

implementation

{ TMemoChange }

procedure TEventMemoChange.SetText(const Value: String);
begin
  FText := Value;
end;

{ TEventCheckBox }

procedure TEventCheckBox.SetChecked(const Value: Boolean);
begin
  FChecked := Value;
end;

end.
