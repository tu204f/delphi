unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indiсator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  FMX.Objects, FMX.Layouts;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    StrGrid: TStringGrid;
    Rectangle: TRectangle;
    TextStatus: TText;
    Layout: TLayout;
    GridLayout: TGridPanelLayout;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    procedure ButtonStartOrStopClick(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  protected
    procedure DoStart;
    procedure DoStop;
  public
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.DateUtils;

constructor TMainForm.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

begin
  inherited;

  SetAddColumn(StrGrid,'Time',150);
  SetAddColumn(StrGrid,'Open');
  SetAddColumn(StrGrid,'High');
  SetAddColumn(StrGrid,'Low');
  SetAddColumn(StrGrid,'Close');
  SetAddColumn(StrGrid,'Vol');

  SetAddColumn(StrGrid,'RSI');
  SetAddColumn(StrGrid,'MaRSI');
  SetAddColumn(StrGrid,'ATR');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_5;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 1;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;
  end;
end;

procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
begin
  // ***********************************************
  // Оценка состояния рынка
  TextStatus.Text :=
    'ValueRSI: ' + TradingPlatform.ValueRSI.RSI.ToString + '; ' +
    'ValueAveragRSI: ' + TradingPlatform.ValueRSI.MovingAveragRSI.ToString + '; ' +
    'ValueATR: ' + TradingPlatform.ValueATR.ATR.ToString  + ';';
end;

end.
