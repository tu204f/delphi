unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Objects,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit, FMX.Layouts;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    StrGrid: TStringGrid;
    RectangleStatus: TRectangle;
    LayoutSatus: TLayout;
    TextParam: TText;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ///<summary>
    /// Событие обновление платформы
    ///</summary>
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    ///<summary>
    /// Получена новая свеча
    ///</summary>
    procedure TradingPlatformOnNewCandel(Sender: TObject);
  protected
    ///<summary>
    /// Старт — получение данных, торговой платформы
    ///</summary>
    procedure DoStart;
    ///<summary>
    /// Стоп
    ///</summary>
    procedure DoStop;

  private
     FFullCount, FPositivCount: Integer;
  public
    ///<summary>
    /// Торговая платформа — работа
    ///</summary>
    TradingPlatform: TTradingPlatform;
    ///<summary>
    /// Индекс событий — обновление
    ///</summary>
    IndexEvent: Integer;
    IsNewCandel: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.History.Candel,
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
  SetAddColumn(StrGrid,'TimeInt',150);
  SetAddColumn(StrGrid,'Open');
  SetAddColumn(StrGrid,'High');
  SetAddColumn(StrGrid,'Low');
  SetAddColumn(StrGrid,'Close');
  SetAddColumn(StrGrid,'Vol');
  SetAddColumn(StrGrid,'color');

  // Торговая платформа — работа TPlatfomBybit bybit
  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;

  // Устанавливаем параметры для работы объекта
  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_60;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Исторические данные, для оценки риска операции.';
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    IndexEvent := 0;
    IsNewCandel := False;

    ButtonStartOrStop.Text := 'Стоп';

    TradingPlatform.Symbol := 'SUIUSDT';
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
var
  xS: String;
  xCandel: TCandel;
  i, iCount: Integer;
  xValues: TDoubleList;

  xValueP, xValueN, xMathValue: Double;
begin
  iCount := AStateMarket.Candels.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := AStateMarket.Candels[i];
      StrGrid.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
      StrGrid.Cells[1,i] := xCandel.Time.ToString;
      StrGrid.Cells[2,i] := xCandel.Open.ToString;
      StrGrid.Cells[3,i] := xCandel.High.ToString;
      StrGrid.Cells[4,i] := xCandel.Low.ToString;
      StrGrid.Cells[5,i] := xCandel.Close.ToString;
      StrGrid.Cells[6,i] := xCandel.Vol.ToString;
      StrGrid.Cells[7,i] := GetStrToTypeCandel(xCandel.TypeCandel);
    end;

  xValues := TDoubleList.Create;
  try
    SetHistoryCandel(AStateMarket.Candels, xValues, FFullCount, FPositivCount);

    xS := 'Null';
    if FFullCount > 0 then
    begin
      xValueP := FPositivCount/FFullCount;
      xValueN := 1 - xValueP;
      xS := 'Кол:' + FFullCount.ToString + '; Positiv: ' + FPositivCount.ToString + '; ';

      xMathValue := GetMathCandel(xValues, xValueP, xValueN);
      xS := xS + 'Мат. ожидание:' + xMathValue.ToString;
    end;
    TextParam.Text := xS;

  finally
    FreeAndNil(xValues);
  end;

end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
begin
  {todo: новая свеча}
end;

end.
