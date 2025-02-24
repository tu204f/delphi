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
    TextEventStatus: TText;
    LayoutSatus: TLayout;
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

  // Торговая платформа — работа TPlatfomBybit bybit
  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;

  // Устанавливаем параметры для работы объекта
  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_1;
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

  procedure _StatusEvent;
  begin
    Inc(IndexEvent);
    TextEventStatus.Text := FormatDateTime('hh.nn.ss.zzz',Time) + ' ' + IndexEvent.ToString;
  end;

var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  _StatusEvent;

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
    end;
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
const
  FILE_NAME = 'source.csv';

  procedure _LineText(S: String);
  var
    F: TextFile;
    xPath: String;
  begin
    xPath := ExtractFilePath(ParamStr(0)) + FILE_NAME;
    AssignFile(f,xPath);
    if FileExists(xPath) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, S);
    CloseFile(F);
  end;

  procedure _LineCande(ACandel: TCandel);
  var
    xS: String;
  begin
    xS := DateTimeToStr(UnixToDateTime(ACandel.Time)) + ';';
    xS := xS + ACandel.Open.ToString + ';';
    xS := xS + ACandel.High.ToString + ';';
    xS := xS + ACandel.Low.ToString + ';';
    xS := xS + ACandel.Close.ToString + ';';
    xS := xS + ACandel.Vol.ToString + ';';
    _LineText(xS);
  end;

var
  xStateMarket: TStateMarket;
  xCandel: TCandel;
  i, iCount: Integer;
begin
  xStateMarket := TradingPlatform.StateMarket;

  iCount := xStateMarket.Candels.Count;
  if iCount > 0 then
  begin
    if IsNewCandel then
    begin
      xCandel := xStateMarket.Candels[0];
      _LineCande(xCandel);
    end
    else
    begin
      IsNewCandel := True;
      for i := iCount - 1 downto 0 do
      begin
        xCandel := xStateMarket.Candels[i];
        _LineCande(xCandel);
      end;
    end;
  end;
end;

end.
