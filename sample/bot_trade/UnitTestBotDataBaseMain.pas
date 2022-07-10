unit UnitTestBotDataBaseMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.ListBox,
  System.Generics.Collections,
  Data.DB, FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FMX.Memo.Types, FMX.Memo,
  Lb.Candel.Source, Lb.Candel.SysUtils;

type
  TMainForm = class(TForm)
    ComboBoxSecurity: TComboBox;
    Text1: TText;
    ComboBoxData: TComboBox;
    Text2: TText;
    ButtonStart: TButton;
    StringGridResult: TStringGrid;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure ComboBoxSecurityChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure Start;
    procedure Stop;

    function GetSecurityID: Integer;
    function GetDateCandels: String;

    procedure IterationCandel(const ACandel: TCandel);
    procedure StopAllBot;
    procedure ResultGrid;

    ///<summary>Первый день</summary>
    procedure FirstDay;
    ///<summary>Следующий день</summary>
    procedure NextDay;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.DataModuleDB, Lb.SysUtils.ISO860, BotTrade, UnitBotTrades;

{todo: Фиксировать состоние каждого бота }
{todo: Количество ботов которые просидали вообще/ и по уровням просадки/количетсо стоп профитов}
{todo: Запусить движение бота}

type
  TParam = record
    ID: Integer;
    Name: String;
  end;
  TSecuritys = TList<TParam>;
  TDates = TList<TDate>;

var
  localDataModuleDB: TDataModuleDB = nil;
  localSecuritys: TSecuritys = nil;
  localDates: TDates = nil;

var
  localIndexDate: Integer = 0;
  localIndexCandel: Integer = 0;
  localSource: TCandelList = nil;


function GetDataModuleDB: TDataModuleDB;
begin
  if not Assigned(localDataModuleDB) then
  begin
    localDataModuleDB := TDataModuleDB.Create(nil);
    localDataModuleDB.DefaultConnection('candels.db');
  end;
  Result := localDataModuleDB;
end;

procedure SetSelectedSecurity(const ASecuritys: TSecuritys);
const
  SQL = 'select * from security';
var
  xParam: TParam;
  xDataSet: TDataSet;
begin
  ASecuritys.Clear;
  xDataSet := GetDataModuleDB.GetSelectCreateDataSet(SQL);
  if Assigned(xDataSet) then
  begin
    xDataSet.First;
    while not xDataSet.Eof do
    begin
      xParam.ID := xDataSet.FieldByName('id').AsInteger;
      xParam.Name := xDataSet.FieldByName('name').AsString;
      xDataSet.Next;
      ASecuritys.Add(xParam);
    end;
    FreeAndNil(xDataSet);
  end;
end;

procedure SetSelectedDates(const ASecurityID: Integer; const ADates: TDates);
const
  SQL = 'select distinct c.date from candel c where c.security_id = :security_id';
var
  xDate:  TDate;
  xDataSet: TDataSet;
begin
  ADates.Clear;
  xDataSet := GetDataModuleDB.GetSelectCreateDataSet(SQL,[ASecurityID],[ftInteger]);
  if Assigned(xDataSet) then
  begin
    xDataSet.First;
    while not xDataSet.Eof do
    begin
      xDate := GetStrISO860ToDate(xDataSet.FieldByName('date').AsString);
      xDataSet.Next;
      ADates.Add(xDate);
    end;
    FreeAndNil(xDataSet);
  end;
end;

procedure GetSelectedCandels(const ASecurityID: Integer; const ADate: String; const ASource: TCandelList);
const
  SQL = 'select c.* ' +
        'from candel c where c.security_id = :security_id and c.date = :date';
var
  xCandel: TCandel;
  xDataSet: TDataSet;
begin
  ASource.Clear;
  xDataSet := GetDataModuleDB.GetSelectCreateDataSet(SQL,[ASecurityID,ADate],[ftInteger,ftString]);
  if Assigned(xDataSet) then
  begin
    xDataSet.First;
    while not xDataSet.Eof do
    begin
      with xCandel do
      begin
        Date := GetStrISO860ToDate(xDataSet.FindField('date').AsString);
        Time := GetStrISO860ToTime(xDataSet.FindField('time').AsString);
        Open := xDataSet.FindField('open').AsFloat;
        High := xDataSet.FindField('high').AsFloat;
        Low  := xDataSet.FindField('low').AsFloat;
        Close := xDataSet.FindField('close').AsFloat;
        Vol  := xDataSet.FindField('value').AsFloat;
      end;
      xDataSet.Next;
      ASource.Add(xCandel);
    end;
    FreeAndNil(xDataSet);
  end;
end;

procedure SetClearPositionBots(const ASecurityID: Integer);
const
  SQL = 'delete from bot where security_id = :security_id';
begin
  GetDataModuleDB.GetExecSQL(SQL,[ASecurityID],[ftInteger]);
end;

procedure SetInsertBot(const ASecurityID: Integer; const ADate: String; const ABot: TDefaultBot);

begin
  var SQL := 'insert into bot (id,profit,stop,result,count_position,min_position,max_position,date,security_id) ' +
             'values(:id,:profit,:stop,:result,:count_position,:min_position,:max_position,:date,:security_id)';
  GetDataModuleDB.GetExecSQL(SQL,
    [ABot.ID, Abot.ValueProfit, ABot.ValueStop, ABot.GetProfitResult, ABot.Positions.Count, ABot.MinPosition, ABot.MaxPosition, ADate, ASecurityID],
    [ftInteger, ftFloat, ftFloat, ftFloat, ftInteger, ftFloat, ftFloat, ftString, ftInteger]
  );
end;

var
  IndexStringColumn: Integer = 0;

procedure CreateStringColumn(const ATitle: String; AGrid: TStringGrid);
var
  xColumn: TStringColumn;
begin
  Inc(IndexStringColumn);
  xColumn := TStringColumn.Create(AGrid);
  xColumn.Name := 'column_name_' + IntToStr(IndexStringColumn);
  xColumn.Header := ATitle;
  xColumn.Parent := AGrid;
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Self.Caption := 'Программа для поиска оптимальной торговой стратегии';
end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetSelectedSecurity(localSecuritys);
  for var xParam in localSecuritys do
    ComboBoxSecurity.Items.Add(xParam.Name);
  ComboBoxSecurity.ItemIndex := 0;

  CreateStringColumn('ID',StringGridResult);
  CreateStringColumn('Profit',StringGridResult);
  CreateStringColumn('Stop',StringGridResult);
  CreateStringColumn('Result',StringGridResult);
  CreateStringColumn('CountPosition',StringGridResult);

  CreateStringColumn('MinPosition',StringGridResult);
  CreateStringColumn('MaxPosition',StringGridResult);

end;

procedure TMainForm.ComboBoxSecurityChange(Sender: TObject);

  procedure _LoadDateComboBox(const ADates: TDates);
  begin
    ComboBoxData.Items.Clear;
    for var d in ADates do
      ComboBoxData.Items.Add(DateToStr(d));
    ComboBoxData.ItemIndex := 0;
  end;

var
  xParam: TParam;
  xIndex: Integer;
begin
  xIndex := ComboBoxSecurity.ItemIndex;
  if xIndex >= 0 then
  begin
    xParam := localSecuritys[xIndex];
    SetSelectedDates(xParam.ID,localDates);
    _LoadDateComboBox(localDates);
  end;
end;

procedure TMainForm.Start;
begin
  TApiBots.StartWork(50,10,1000,50,10,1000);

  Self.Caption := 'Программа для поиска оптимальной торговой стратегии: CountBot = ' + TApiBots.Bots.Count.ToString;

  ButtonStart.Text := 'Стоп';
  Timer.Enabled := True;
  FirstDay;
end;

procedure TMainForm.Stop;
begin
  ButtonStart.Text := 'Старт';
  Timer.Enabled := False;
  Self.StopAllBot;
end;

function TMainForm.GetDateCandels: String;
begin
  Result := '';
  if localIndexDate >= 0 then
    Result := GetDateToStrISO860(localDates[localIndexDate]);
end;

function TMainForm.GetSecurityID: Integer;
begin
  Result := -1;
  var xIndex := ComboBoxSecurity.ItemIndex;
  if xIndex >= 0 then
  begin
    var xParam := localSecuritys[xIndex];
    Result := xParam.ID;
  end;
end;

procedure TMainForm.FirstDay;
var
  xSecurityID: Integer;
  xDate: String;
begin
  localIndexDate := 0;
  localIndexCandel := 0;

  ComboBoxData.ItemIndex := localIndexDate;

  TApiBots.ExecutionStart;

  xSecurityID := GetSecurityID;
  xDate := GetDateCandels;
  GetSelectedCandels(xSecurityID, xDate, localSource);
end;

procedure TMainForm.NextDay;

  procedure _FixBot;
  begin
    var xSecurityID := GetSecurityID;
    var xDate := GetDateCandels;

    for var xBot in TApiBots.Bots do
      SetInsertBot(xSecurityID,xDate,xBot);

  end;

begin
  _FixBot;

  Inc(localIndexDate,1);
  localIndexCandel := 0;

  if localIndexDate < localDates.Count then
  begin
    ComboBoxData.ItemIndex := localIndexDate;
    var xSecurityID := GetSecurityID;
    var xDate := GetDateCandels;
    TApiBots.ExecutionStart;

    GetSelectedCandels(xSecurityID, xDate, localSource);
  end
  else
  begin
    TApiBots.ExecutionStop;
  end;
end;

procedure TMainForm.ResultGrid;
begin
  // Выводить показателе в таблицу
  StringGridResult.BeginUpdate;
  var iCount := TApiBots.Bots.Count;
  StringGridResult.RowCount := iCount;
  for var i := 0 to iCount - 1 do
  begin
    StringGridResult.Cells[0,i] := TApiBots.Bots[i].ID.ToString;
    StringGridResult.Cells[1,i] := TApiBots.Bots[i].ValueProfit.ToString;
    StringGridResult.Cells[2,i] := TApiBots.Bots[i].ValueStop.ToString;
    StringGridResult.Cells[3,i] := TApiBots.Bots[i].GetProfitResult.ToString;
    StringGridResult.Cells[4,i] := TApiBots.Bots[i].Positions.Count.ToString;

    StringGridResult.Cells[5,i] := TApiBots.Bots[i].MinPosition.ToString;
    StringGridResult.Cells[6,i] := TApiBots.Bots[i].MaxPosition.ToString;

  end;
  StringGridResult.EndUpdate;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Запускаем несколько объектов - подряд, в потоках
  if Timer.Enabled then
    Stop
  else
    Start;
end;

procedure TMainForm.IterationCandel(const ACandel: TCandel);
begin
  TApiBots.ExecutionLastCandel(ACandel);
end;

procedure TMainForm.StopAllBot;
begin
  TApiBots.ExecutionStop;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xCandel: TCandel;
begin
  try
    xCandel := localSource[localIndexCandel];
    Self.IterationCandel(xCandel);
    // Следующая свеча
    Inc(localIndexCandel,1);
    // Как Список закончилься
    if localIndexCandel >= localSource.Count then
    begin
      StopAllBot; // Принудительное закрытие позиции
      NextDay;    // Берес следуюший день
    end;

    ResultGrid;
  except
    raise Exception.Create('Error Message: ' + localIndexCandel.ToString + ' ' +  localSource.Count.ToString);
  end;

end;

initialization
  localSecuritys := TSecuritys.Create;
  localDates := TDates.Create;
  localSource := TCandelList.Create;

finalization
  FreeAndNil(localSource);
  FreeAndNil(localSecuritys);
  FreeAndNil(localDataModuleDB);

end.
