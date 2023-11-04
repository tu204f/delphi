unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Lb.SysUtils;

type
  TMainForm = class(TForm)
    ComboBoxTrades: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBoxTikers: TListBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxTradesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    InfoUsers: TInfoUsers;
    MarketTrades: TMarketTrades;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InfoUsers := TInfoUsers.Create;
  MarketTrades := TMarketTrades.Create;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InfoUsers.Load;
  InfoUsers.NameTraders(ComboBoxTrades.Items);
end;

procedure TMainForm.ComboBoxTradesClick(Sender: TObject);
var
  xIndex: Integer;
  xInfo: TInfoUser;
begin
  xIndex := ComboBoxTrades.ItemIndex;
  xInfo := InfoUsers[xIndex];
  Label2.Caption := Format('[%s]. Доходность: ',[xInfo.ID]) + xInfo.Profit.ToString;
  MarketTrades.LoadID(xInfo.ID);
  MarketTrades.SetNameTikers(ListBoxTikers.Items);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(MarketTrades);
  FreeAndNil(InfoUsers);
end;

procedure TMainForm.Button1Click(Sender: TObject);

  procedure _SaveSCV;
  var
    xS: String;
    xStr: TStrings;
  begin
    xStr := TStringList.Create;
    try
      for var xTiket in MarketTrades.Tikers do
      begin
        xS := Format('%s;%d',[xTiket.Name,xTiket.Count]);
        xStr.Add(xS);
      end;
      xStr.SaveToFile('d:\work\git\delphi\sample\traders\tikers.csv');
    finally
      FreeAndNil(xStr);
    end;
  end;

begin
  MarketTrades.Clear;
  for var xInfo in InfoUsers do
    MarketTrades.LoadNonClearID(xInfo.ID);
  MarketTrades.SetNameTikers(ListBoxTikers.Items);
  _SaveSCV;
end;

end.
