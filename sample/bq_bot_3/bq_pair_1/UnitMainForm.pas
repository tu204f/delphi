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
  FMX.Layouts,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Procs,
  FMXTee.Chart,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    Chart: TChart;
    SeriesRate: TLineSeries;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    ChartPrice: TChart;
    SeriesF: TLineSeries;
    SeriesS: TLineSeries;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    QtyFutures: Double;
    QtySpot: Double;
  private
    FSecFutures: TBybitKline;
    FSecSpot: TBybitKline;
    procedure SecFuturesOnEventEndLoading(Sender: TObject);
    procedure SecSpotOnEventEndLoading(Sender: TObject);
  protected
    procedure SetCompiledQty;
    procedure SetCompiledPrice;
  public
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
  FSecFutures := TBybitKline.Create;
  FSecSpot := TBybitKline.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FSecSpot);
  FreeAndNil(FSecFutures);
  inherited;
end;

procedure TMainForm.SecFuturesOnEventEndLoading(Sender: TObject);
begin
  Memo1.Lines.Add('Futures.End');
end;

procedure TMainForm.SecSpotOnEventEndLoading(Sender: TObject);
begin
  Memo1.Lines.Add('Spot.End');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Старт');

  FSecFutures.Symbol := 'BTCUSDT-30MAY25';
  FSecFutures.Interval := TTypeInterval.ti_15;
  FSecFutures.Limit := 1000;
  FSecFutures.Selected;
  FSecFutures.OnEventEndLoading := SecFuturesOnEventEndLoading;

  FSecSpot.Symbol := 'BTCUSDT';
  FSecSpot.Interval := TTypeInterval.ti_15;
  FSecSpot.Limit := 1000;
  FSecSpot.Selected;
  FSecSpot.OnEventEndLoading := SecSpotOnEventEndLoading;
end;


procedure TMainForm.Button2Click(Sender: TObject);
begin
  SetCompiledQty;
  SetCompiledPrice;
end;

function GetQty(const ADepo, APrice: Double): Double;
var
  xValue: Double;
begin
  xValue := ADepo/APrice;
  xValue := Round(xValue * 100)/100;
  Result := xValue;
end;

function GetBybitKline(ABybitKline: TBybitKline; ADepo: Double): Double;
var
  iCount: Integer;
  xFirstPrice, xLastPrice: Double;
  xQty, xFirstQty, xLastQty: Double;
begin
  iCount := ABybitKline.CandelObjects.Count;

  xFirstPrice := ABybitKline.CandelObjects[0].Close;
  xLastPrice := ABybitKline.CandelObjects[iCount - 1].Close;

  xFirstQty := GetQty(ADepo, xFirstPrice);
  xLastQty  := GetQty(ADepo, xLastPrice);

  xQty := (xFirstQty + xLastQty)/2;
  xQty := Round(xQty * 100)/100;
  Result := xQty;
end;

procedure TMainForm.SetCompiledQty;
begin
  Memo1.Lines.Add('**********************************************************');
  QtyFutures := GetBybitKline(FSecFutures,1000);
  Memo1.Lines.Add('Futures.Qty :='  + QtyFutures.ToString);

  Memo1.Lines.Add('**********************************************************');
  QtySpot := GetBybitKline(FSecSpot,1000);
  Memo1.Lines.Add('Stop.Qty :='  + QtySpot.ToString);
end;

procedure TMainForm.SetCompiledPrice;
var
  xValue: Double;
  i, iCount: Integer;
  xCandelSpot, xCandelFutures: TCandelObject;
var
  xPriceF, xPriceS: Double;
begin
  SeriesF.Clear;
  SeriesS.Clear;
  SeriesRate.Clear;
  iCount := FSecFutures.CandelObjects.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandelFutures := FSecFutures.CandelObjects[i];
      xCandelSpot := FSecSpot.CandelObjects[i];

      xPriceF := xCandelFutures.Close * QtyFutures;
      SeriesF.AddY(xPriceF);

      xPriceS := xCandelSpot.Close * QtySpot;
      SeriesS.AddY(xPriceS);



      xValue := xCandelFutures.Close * QtyFutures - xCandelSpot.Close * QtySpot;
      SeriesRate.AddY(xValue);
    end;
end;

end.
