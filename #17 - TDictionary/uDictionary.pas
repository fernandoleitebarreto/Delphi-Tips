unit uDictionary;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinsDefaultPainters, cxTextEdit, cxCurrencyEdit;

type
  TCity = class
    Country: String;
    Latitude: Double;
    Longitude: Double;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    btnAdd: TButton;
    edCity: TEdit;
    edCountry: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnPrintDictionary: TButton;
    btnCount: TButton;
    btnTryGetValue: TButton;
    btnRemove: TButton;
    btnTrimExcess: TButton;
    btnContainsKey: TButton;
    btnContainsValue: TButton;
    btnAddOrSetValue: TButton;
    btnClear: TButton;
    Label5: TLabel;
    edLatitude: TEdit;
    edLongitude: TEdit;
    procedure KNotify(Sender: TObject; const Item: String;
      Action: TCollectionNotification);
    procedure VNotify(Sender: TObject; const Item: TCity;
      Action: TCollectionNotification);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAddClick(Sender: TObject);
    procedure btnPrintDictionaryClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnTryGetValueClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnTrimExcessClick(Sender: TObject);
    procedure btnContainsKeyClick(Sender: TObject);
    procedure btnContainsValueClick(Sender: TObject);
    procedure btnAddOrSetValueClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
    Dictionary: TDictionary<String, TCity>;
    City: TCity;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{
  This example demonstrates the usage of the main methods
  and properties in TDictionary.
}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Create the dictionary }
  Dictionary := TDictionary<String, TCity>.Create;
  City := TCity.Create;
  Dictionary.OnKeyNotify := KNotify;
  Dictionary.OnValueNotify := VNotify;
end;

procedure TForm1.KNotify(Sender: TObject; const Item: String;
  Action: TCollectionNotification);
begin
  { Show a message each time a key is added or removed }
  Memo1.Lines.Add('The key "' + Item + '" was added/removed.');

end;

procedure TForm1.VNotify(Sender: TObject; const Item: TCity;
  Action: TCollectionNotification);

begin
  { Show a message each time a value is added or removed }
  Memo1.Lines.Add('The value "' + Item.Country + '" was added/removed.');

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;

end;

procedure TForm1.btnPrintDictionaryClick(Sender: TObject);
var
  Value: TCity;
  Key: String;
begin
  { Iterate through all keys in the dictionary and display their coordinates }
  Memo2.Lines.Clear;

  for Key in Dictionary.Keys do
  begin
    Memo2.Lines.Add(#13 + 'Key:' + Key);
    Memo2.Lines.Add(#13 + 'Dictionary.Items[Key].Country:' + Dictionary.Items
      [Key].Country);
    Memo2.Lines.Add(#13 + 'Dictionary.Items[Key].Latitude:' +
      FloatToStrF(Dictionary.Items[Key].Latitude, ffFixed, 4, 2));
    Memo2.Lines.Add(#13 + 'Dictionary.Items[Key].Longitude: ' +
      FloatToStrF(Dictionary.Items[Key].Longitude, ffFixed, 4, 2));
    Memo2.Lines.Add('----------------------------------------');
  end;

  for Value in Dictionary.Values do
  begin
    Memo2.Lines.Add(#13 + 'Country:' + Value.Country);
    Memo2.Lines.Add(#13 + 'Latitude:' + FloatToStrF(Value.Latitude,
      ffFixed, 4, 2));
    Memo2.Lines.Add(#13 + 'Longitude: ' + FloatToStrF(Value.Longitude,
      ffFixed, 4, 2));
    Memo2.Lines.Add('----------------------------------------');
  end;

end;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  Memo1.Lines.Add(' Add some key-value pairs to the dictionary');
  City.Country := edCountry.Text;
  City.Latitude := StrToFloat(edLatitude.Text);
  City.Longitude := StrToFloat(edLongitude.Text);

  try
    Dictionary.Add(edCity.Text, City);
  except
    on Exception do
      Memo1.Lines.Add('Could not add entry. Duplicates are not allowed.');
  end;
end;

procedure TForm1.btnCountClick(Sender: TObject);
begin
  { Display the current number of key-value entries }
  Memo1.Lines.Add('Number of pairs in the dictionary: ' +
    IntToStr(Dictionary.Count));
end;

procedure TForm1.btnTryGetValueClick(Sender: TObject);
begin
  if edCity.Text = '' then
  begin
    ShowMessage('City is empty');
    if edCity.CanFocus then
      edCity.SetFocus;
  end
  else
  begin
    if (Dictionary.TryGetValue(edCity.Text, City) = True) then
    begin
      Memo1.Lines.Add(edCity.Text + ' is located in ' + City.Country +
        ' with latitude = ' + FloatToStrF(City.Latitude, ffFixed, 4, 2) +
        ' and longitude = ' + FloatToStrF(City.Longitude, ffFixed, 4, 2));
    end
    else
      Memo1.Lines.Add('Could not find ' + edCity.Text + ' in the dictionary');
  end;
end;

procedure TForm1.btnRemoveClick(Sender: TObject);
begin
  if edCity.Text = '' then
  begin
    ShowMessage('City is empty');
    if edCity.CanFocus then
      edCity.SetFocus;
  end
  else
  begin
    { Remove the city key from dictionary }
    Dictionary.Remove(edCity.Text);
  end;
end;

procedure TForm1.btnTrimExcessClick(Sender: TObject);
begin
  { Make sure the dictionary's capacity is set to the no. of entries }
  Dictionary.TrimExcess;
end;

procedure TForm1.btnContainsKeyClick(Sender: TObject);
begin
  if edCity.Text = '' then
  begin
    ShowMessage('City is empty');
    if edCity.CanFocus then
      edCity.SetFocus;
  end
  else
  begin
    { Test if "city" is a key in the dictionary }
    if Dictionary.ContainsKey(edCity.Text) then
      Memo1.Lines.Add('The key "' + edCity.Text + '" is in the dictionary.')
    else
      Memo1.Lines.Add('The key "' + edCity.Text +
        '" is not in the dictionary.');
  end;
end;

procedure TForm1.btnContainsValueClick(Sender: TObject);
//var
//  City2: TCity;
begin
  { Test if (Argentina, 0, 0) is a value in the dictionary }
//  City2 := TCity.Create;
  City.Country := edCountry.Text;
  City.Latitude := StrToFloat(edLatitude.Text);
  City.Longitude := StrToFloat(edLongitude.Text);


  btnPrintDictionaryClick(Sender);
  if Dictionary.ContainsValue(City) then
    Memo1.Lines.Add('The value (' + edCountry.Text + ', ' + edLatitude.Text +
      ', ' + edLongitude.Text + ') is in the dictionary.')
  else
    Memo1.Lines.Add('The value (' + edCountry.Text + ', ' + edLatitude.Text +
      ', ' + edLongitude.Text + ') is not in the dictionary.');

//  FreeAndNil(City2);
end;

procedure TForm1.btnAddOrSetValueClick(Sender: TObject);
begin
  { Update the coordinates to the correct ones }
  City.Country := edCountry.Text;
  City.Latitude := StrToFloat(edLatitude.Text);
  City.Longitude := StrToFloat(edLongitude.Text);
  Dictionary.AddOrSetValue(edCity.Text, City);
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  { Clear all entries in the dictionary }
  Dictionary.Clear;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Free the memory allocated for the dictionary }
  Dictionary.Destroy;
end;

end.
