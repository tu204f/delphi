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
  Lb.Bot,
  Lb.Platfom,
  Lb.Platfom.Bybit;

type
  TMainForm = class(TForm)
  private
    { Private declarations }
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

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Bot := TBot.Create;
  TradingPlatform := TPlatfomBybit.Create;

  Bot.TradingPlatform := TradingPlatform;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  FreeAndNil(Bot);
  inherited;
end;

end.
