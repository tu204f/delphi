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
  private
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
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(BybitKline);
  inherited;
end;


procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  BybitKline.Category := TTypeCategory.tcLinear;
  BybitKline.Symbol := 'ETHUSDT';
  //BybitKline.Symbol := 'BTCUSDT';
  BybitKline.Interval := TTypeInterval.ti_D;
  //BybitKline.StartTime := 1718402400000;
  //BybitKline.EndTime   := 1718402400000 - 5000;
  BybitKline.Limit := 3000;
  BybitKline.Selected;
end;

procedure TMainForm.BybitKlineOnEventEndLoading(Sender: TObject);
var
  xCandel: TCandelObject;
  i, iCount, xInd: Integer;
  xS: String;
  xStr: TStrings;
begin
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
          xCandel.startTime + ';' +
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
    xStr.SaveToFile('data_' + BybitKline.Symbol + '.csv');
  end;
end;

end.
