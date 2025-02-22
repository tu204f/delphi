unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  FMX.Objects;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    StrGrid: TStringGrid;
    TextStatus: TText;
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
    TradingPlatform.StateMarket.Qty := 0.2;
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
var
  xCandel: TCandel;
  i, iCount: Integer;
begin

  iCount := AStateMarket.Candels.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := AStateMarket.Candels[i];
      StrGrid.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
      StrGrid.Cells[1,i] := xCandel.Open.ToString;
      StrGrid.Cells[2,i] := xCandel.High.ToString;
      StrGrid.Cells[3,i] := xCandel.Low.ToString;
      StrGrid.Cells[4,i] := xCandel.Close.ToString;
      StrGrid.Cells[5,i] := xCandel.Vol.ToString;
      StrGrid.Cells[6,i] := TradingPlatform.ValueRSI.ValueRSI[i].ToString;
      StrGrid.Cells[7,i] := TradingPlatform.ValueRSI.ValueMaRSI[i].ToString;
      StrGrid.Cells[8,i] := TradingPlatform.ValueATR.Values[i].ToString;
    end;

  TextStatus.Text :=
    'ValueRSI: ' + TradingPlatform.ValueRSI.RSI.ToString + '; ' +
    'ValueAveragRSI: ' + TradingPlatform.ValueRSI.MovingAveragRSI.ToString + '; ' +
    'ValueATR: ' + TradingPlatform.ValueATR.ATR.ToString  + ';';
end;

end.
