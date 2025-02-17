unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indiñator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  Lb.BlackBox,

  FMX.Objects,
  FMX.Ani;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    StrGrid: TStringGrid;
    TextStatus: TText;
    Button1: TButton;
    BitmapAnimation1: TBitmapAnimation;
    ButtonCreateBox: TButton;
    BoxGrid: TStringGrid;
    Button2: TButton;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonCreateBoxClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
  protected
    Boxs: TBoxList;
    procedure DoStart;
    procedure DoStop;
  protected
    procedure EventBoxStart(Sender: TObject);
    procedure EventBoxStop(Sender: TObject);
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


  SetAddColumn(BoxGrid,'ID');
  SetAddColumn(BoxGrid,'Status');
  SetAddColumn(BoxGrid,'Val');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;

  TPlatfomBybit(TradingPlatform).ApiKey := '3bvDxJnKzjkIg8y0RV';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'YtpORO6EYWTESXWwCyLiOBm75c1Tv6GSOzqJ';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_5;

  Boxs := TBoxList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Boxs);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Ñòîï';

    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 0.2;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Ñòàðò';

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

procedure TMainForm.ButtonCreateBoxClick(Sender: TObject);

  procedure _CreateBoxs;
  var
    i, iCount: Integer;
    xCandel: TCandel;
    xCandels: TCandelList;
    xBuffers: TCandelList;
  var
    xBox: TBox;
  begin
    xCandels := TradingPlatform.StateMarket.Candels;
    iCount := xCandels.Count;
    if iCount > 0 then
    begin
      xBuffers := TCandelList.Create;
      try
        for i := 0 to iCount - 1 do
        begin
          xCandel := xCandels[i];
          xBuffers.Add(xCandel);
          if xBuffers.Count >= 10 then
          begin
            xBox := TBox.Create;
            xBox.SetUneCandels(xBuffers);
            xBox.ID := Boxs.Count;
            Boxs.Add(xBox);

            xBox.OnStart := EventBoxStart;
            xBox.OnStop := EventBoxStop;

            xBuffers.Delete(0);
          end;
        end;
      finally
        FreeAndNil(xBuffers);
      end;
    end;
  end;

  procedure _BoxsGrid;
  var
    xBox: TBox;
    i, iCount: Integer;
  begin
    BoxGrid.RowCount := 0;
    iCount := Boxs.Count;
    if iCount > 0 then
    begin
      BoxGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xBox := Boxs[i];
        BoxGrid.Cells[0,i] := (i + 1).ToString;
        BoxGrid.Cells[1,i] := 'non';
        xBox.NeuroThread.Start;
      end;
    end;
  end;

begin
  _CreateBoxs;
  _BoxsGrid;
end;

procedure TMainForm.Button2Click(Sender: TObject);

  procedure _BoxsGrid;
  var
    xBox: TBox;
    i, iCount: Integer;
  begin
    BoxGrid.RowCount := 0;
    iCount := Boxs.Count;
    if iCount > 0 then
    begin
      BoxGrid.RowCount := iCount;
      for i := 0 to iCount - 1 do
      begin
        xBox := Boxs[i];
        BoxGrid.Cells[0,i] := (i + 1).ToString;
        BoxGrid.Cells[1,i] := 'non';
        xBox.NeuroThread.Start;
      end;
    end;
  end;

begin
  _BoxsGrid;
end;

procedure TMainForm.EventBoxStart(Sender: TObject);
var
  xBox: TBox;
begin
  xBox := TBox(Sender);
  BoxGrid.Cells[1,xBox.ID] := 'start';
end;

procedure TMainForm.EventBoxStop(Sender: TObject);
var
  xBox: TBox;
begin
  xBox := TBox(Sender);
  BoxGrid.Cells[1,xBox.ID] := 'stop';

  var xCandels := TradingPlatform.StateMarket.Candels;
  var xBuffers := TCandelList.Create;
  try
    for var i := 1 to 10 do
    begin
      var xCandel := xCandels[i];
      xBuffers.Add(xCandel);
    end;
    xBox.SetUneCandels(xBuffers);
  finally
    FreeAndNil(xBuffers);
  end;
  BoxGrid.Cells[2,xBox.ID] := xBox.NeuroThread.ValueNeuronNet.ToString;

end;

end.
