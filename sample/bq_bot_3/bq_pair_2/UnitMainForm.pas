unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  FMX.StdCtrls,

  UnitPairsFrame,

  Lb.Trading.Pair,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Encryption,
  Lb.Bybit.Tickers;

type
  TMainForm = class(TForm)
    LayoutMemu: TLayout;
    ButtonStart: TButton;
    LayoutStatus: TLayout;
    LayoutClient: TLayout;
    ButtonSell: TButton;
    ButtonBuy: TButton;
    ButtonSave: TButton;
    ButtonLoad: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    FPairsFrame: TPairsFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if FPairsFrame.Tickers.Active then
  begin
    ButtonStart.Text := 'Старт';
    FPairsFrame.Tickers.Stop;
  end else
  begin
    ButtonStart.Text := 'Стоп';
    FPairsFrame.Tickers.Start(100);
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPairsFrame := TPairsFrame.Create(nil);
  FPairsFrame.Parent := LayoutClient;
  FPairsFrame.Align := TAlignLayout.Client;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FPairsFrame);
  inherited;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  FPairsFrame.SetPairBuy;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin
  FPairsFrame.SetPairSell;
end;

procedure TMainForm.ButtonSaveClick(Sender: TObject);
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0));
  FPairsFrame.SetSavePairs(xPath + 'pair.ini');
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0));
  FPairsFrame.SetLoadPairs(xPath + 'pair.ini');
end;

end.
