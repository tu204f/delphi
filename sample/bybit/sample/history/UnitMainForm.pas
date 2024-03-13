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
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline,
  System.Generics.Collections;

type
  TDoubleList = TList<Double>;

  TMainForm = class(TForm)
    StrGrid: TStringGrid;
    ButtonStart: TButton;
    ButtonStop: TButton;
    procedure ButtonStartClick(Sender: TObject);
  protected
    function GetRSI(const AIndex, ACount: Integer): Double;
  private
    ValueRSI: TDoubleList;
    BybitKline: TBybitKline;
    procedure BybitKlineOnEventEndLoading(Sender: TObject);
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

  procedure AddCol(const AHeader: String; const AWidth: Single = 0);
  var
    xC: TStringColumn;
  begin
    xC := TStringColumn.Create(StrGrid);
    xC.Parent := StrGrid;
    xC.Header := AHeader;
    if AWidth > 0 then
      xC.Width  := AWidth;
  end;

begin
  inherited;
  BybitKline := TBybitKline.Create;
  BybitKline.OnEventEndLoading := BybitKlineOnEventEndLoading;

  AddCol('id',50);
  AddCol('startTime',150);
  AddCol('openPrice');
  AddCol('highPrice');
  AddCol('lowPrice');
  AddCol('closePrice');
  AddCol('volume:');
  AddCol('turnover');

  ValueRSI := TDoubleList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ValueRSI);
  FreeAndNil(BybitKline);
  inherited;
end;


procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_5;
  BybitKline.Limit := 1;
  BybitKline.Start(500);
end;

function TMainForm.GetRSI(const AIndex, ACount: Integer): Double;

  function GetMA(const AValues: TDoubleList): Double;
  var
    xSum: Double;
    i, iCount: Integer;
  begin
    Result := 0;
    xSum := 0;
    iCount := AValues.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
        xSum := xSum + AValues[i];
      Result := xSum/iCount;
    end;
  end;

var
  xInd: Integer;
  xDelta: Double;
  xG, xL: TDoubleList;
  i, iCount: Integer;
  xCandel: TCandelObject;
begin
  Result := 0;
  xG := TDoubleList.Create;
  xL := TDoubleList.Create;
  try
    iCount := BybitKline.CandelObjects.Count;
    if iCount > 0 then
    begin
      xInd := iCount - 1;
      for i := AIndex to iCount - 1 do
      begin
        xCandel := BybitKline.CandelObjects[xInd - i];
        xDelta := xCandel.Close - xCandel.Open;
        if xDelta > 0 then
        begin
          xG.Add(xDelta);
          xL.Add(0);
        end
        else
        begin
          xG.Add(0);
          xL.Add(xDelta);
        end;
      end;
    end;
  finally
    FreeAndNil(xL);
    FreeAndNil(xG);
  end;
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xCandel: TCandelObject;
  i, iCount, xInd: Integer;
  xS: String;
  xStr: TStrings;
begin
  ValueRSI.Clear;

  xStr := TStringList.Create;
  try
    StrGrid.RowCount := 0;
    iCount := BybitKline.CandelObjects.Count;
    if iCount > 0 then
    begin
      StrGrid.RowCount := iCount;
      xInd := iCount - 1;
      for i := 0 to iCount - 1 do
      begin
        xCandel := BybitKline.CandelObjects[xInd - i];

        StrGrid.Cells[0,i] := (i + 1).ToString;
        StrGrid.Cells[1,i] := DateTimeToStr(xCandel.DateTime);
        StrGrid.Cells[2,i] := xCandel.openPrice;
        StrGrid.Cells[3,i] := xCandel.highPrice;
        StrGrid.Cells[4,i] := xCandel.lowPrice;
        StrGrid.Cells[5,i] := xCandel.closePrice;
        StrGrid.Cells[6,i] := xCandel.volume;
        StrGrid.Cells[7,i] := xCandel.turnover;

        xS :=
          DateTimeToStr(xCandel.DateTime) + ';' +
          FloatToStr(xCandel.Open) + ';' +
          FloatToStr(xCandel.High) + ';' +
          FloatToStr(xCandel.Low) + ';' +
          FloatToStr(xCandel.Close) + ';' +
          FloatToStr(xCandel.Vol) + ';';

        xStr.Add(xS);
      end;

    end;
  finally
    xStr.SaveToFile('data.csv');
  end;
end;

end.
