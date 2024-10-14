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
  Lb.SysUtils,
  Lb.Bot,
  Lb.Platfom,
  Lb.Platfom.Bybit, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  public
    Bot: TBot;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Bot.TradingPlatform.Symbel := 'ETHUSDT';
  Bot.TradingPlatform.Start;
  Bot.TradingPlatform.StateMarket.Qty := 0.2;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Bot := TBot.Create;
  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  Bot.TradingPlatform := TradingPlatform;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(Bot);
  inherited;
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
var
  xS: String;
begin
  Memo1.BeginUpdate;
  try
    Memo1.Lines.Clear;

    Memo1.Lines.Add(
      'Ask: ' + TradingPlatform.StateMarket.Ask.ToString + '; ' +
      'Bid: ' + TradingPlatform.StateMarket.Bid.ToString
    );
    Memo1.Lines.Add('****************************************************');

    for var xC in AStateMarket.Candels do
    begin
      xS :=
        xC.Time.ToString + '; ' +
        xC.Open.ToString + '; ' +
        xC.High.ToString + '; ' +
        xC.Low.ToString + '; ' +
        xC.Close.ToString + ';  ' +
        xC.Vol.ToString;
      Memo1.Lines.Add(xS);
    end;
  finally
    Memo1.EndUpdate;
  end;
  Bot.SetSelected;
end;

end.
