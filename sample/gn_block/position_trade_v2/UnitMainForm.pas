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
  FMX.Memo.Types,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.StdCtrls,
  Lb.ReadPrice,
  Lb.Pattern,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    Timer: TTimer;
    ButtonStop: TButton;
    ProgressBar: TProgressBar;
    StrGrid: TStringGrid;
    Text1: TText;
    Text2: TText;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  protected
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
    ParrentTrades: TTradeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveParrentTrades;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  TRAND_PATTERN = 100;
  TRAND_PROFIT  = 10;
  VALUE_PROFIT  = 0.3;

{ TMainForm }

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
    Text2.Text := ParrentTrades.Count.ToString;
    SaveParrentTrades;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(AGrid: TStringGrid; AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AGrid);
    xCol.Header := AHeader;
    xCol.Parent := AGrid;
  end;

begin
  inherited;
  CandelsSource := TCandelsSource.Create;
  ParrentTrades := TTradeList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ParrentTrades);
  FreeAndNil(CandelsSource);
  inherited;
end;


procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xFileName: String;
begin
  if not Timer.Enabled then
  begin
    CandelIndex := 0;
    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';

    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240501_240731.csv';
    //xFileName := xFileName + 'GAZP_240601_240804.csv';
    xFileName := xFileName + 'GAZP_240701_240917.csv';
    //xFileName := xFileName + 'SPFB.GAZR-9.24_240701_240919.csv';

    CandelsSource.LoadFromFile(xFileName);
    CandelsSource.Delete(0);
    Timer.Enabled := True;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _Parrent(const ABuySell: Char);
  var
    xCandel: TCandel;
    xParrent: TCandelList;
    xTrade: TTrade;
  begin
    // Создание масски
    xParrent := TCandelList.Create;
    try
      for var i := TRAND_PATTERN - 1 downto 0 do
      begin
        xCandel := CandelsSource.Candels[CandelIndex - i];
        xParrent.Add(xCandel);
      end;

      xTrade := TTrade.Create;
      xTrade.BuySell := ABuySell;
      ToCandelParrent(xParrent,xTrade.ParrentCandels);
      ParrentTrades.Add(xTrade);


    finally
      FreeAndNil(xParrent);
    end;
  end;

  procedure _Timer;
  var
    xCurrentCandel, xProfitCandel: TCandel;
  begin
    try
      if CandelIndex > TRAND_PATTERN then
      begin
        xCurrentCandel := CandelsSource.Candels[CandelIndex];
        for var i := 1 to TRAND_PROFIT do
        begin
          xProfitCandel  := CandelsSource.Candels[CandelIndex + i];
          // Покупка
          if (xProfitCandel.High - xCurrentCandel.Close) >= VALUE_PROFIT then
          begin
            _Parrent('B');
            Break;
          end;

          // Продать
          if (xCurrentCandel.Close - xProfitCandel.Low) >= VALUE_PROFIT then
          begin
            _Parrent('S');
            Break;
          end;
        end;
      end;

      Text1.Text := CandelIndex.ToString;
      ProgressBar.Value := 100 * CandelIndex/(CandelsSource.Count);
      Inc(CandelIndex);
    except
      ButtonStopClick(nil);
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to 99 do
  begin
    if CandelsSource.Count > (CandelIndex + TRAND_PROFIT) then
      _Timer
    else
    begin
      ButtonStopClick(nil);
      Break;
    end;
  end;
end;

procedure TMainForm.SaveParrentTrades;
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xStr.Add(ParrentTrades.Count.ToString);
    var xFileName := ExtractFilePath(ParamStr(0)) + 'source\';
    xFileName := xFileName + 'param.mps';
    xStr.SaveToFile(xFileName);
  finally
    FreeAndNil(xStr);
  end;


  for var i := 0 to ParrentTrades.Count - 1 do
  begin



    var xTrade := ParrentTrades[i];
    xStr := TStringList.Create;
    try
      StringsToTrade(xTrade,xStr);
      var xFileName := ExtractFilePath(ParamStr(0)) + 'source\';
      xFileName := xFileName + i.ToString + '.mps';
      xStr.SaveToFile(xFileName);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

end.
