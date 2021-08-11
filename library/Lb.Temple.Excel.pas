unit Lb.Temple.Excel;

interface

{$I situat_maps.inc}

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Command;

type
  TWorkBook = class;

  /// <summary>
  /// Лист книги
  /// </summary>
  TWorkSheet = class(TObject)
  private
    FWorkSheet: OleVariant;
    FRowCount: Integer;
    FColCount: Integer;
    FValue: Variant;
    FName: String;
    FWorkBook: TWorkBook;
  protected
    procedure SetInitialization;
    procedure SetCommands;
  private
    function GetRowHieght(Row: Integer): Integer;
    procedure SetRowHeight(Row: Integer; const Value: Integer);
  protected
    procedure SetRowMinHieght(Row: Integer);
    property RowHieght[Row: Integer]: Integer read GetRowHieght write SetRowHeight;
  public
    constructor Create(const AWorkBook: TWorkBook; const AWorkSheet: OleVariant); virtual;
    destructor Destroy; override;
    /// <summary>
    /// Количество занимаемых строк
    /// </summary>
    property RowCount: Integer read FRowCount;
    /// <summary>
    /// Количестов занимаемых колонок
    /// </summary>
    property ColCount: Integer read FColCount;
    /// <summary>
    /// Данные которы там содержутся
    /// </summary>
    property Value: Variant read FValue;
    /// <summary>
    /// Имя листа
    /// </summary>
    property Name: String read FName;
  public
    function ToString: string; override;
  end;

  /// <summary>
  /// Список листов
  /// </summary>
  TWorkSheetList = TObjectList<TWorkSheet>;

  /// <summary>
  /// Создаем или открываем книгу
  /// </summary>
  TWorkBook = class(TObject)
  private
    FAppExcelVisible: Boolean;
    FAppExcelQuit: Boolean;
  private
    FFileName: String;
    FAppProgID: String;
    FAppExcel: OleVariant;
    FWorkbook: OleVariant;
  private
    FCommands: TCommands;
    FWorkSheets: TWorkSheetList;
  protected
    procedure SetWorkSheet(AWorkSheets: OleVariant);
    procedure SetInitializationExcel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Commands: TCommands read FCommands write FCommands;
    // ------------------------------------------------------------------------
    procedure Open; overload;
    procedure Open(const AFileName: String); overload;
    procedure Open(const AFileName: String; AVisible: Boolean); overload;
    /// <summary>
    /// Открыть Excel
    /// </summary>
    /// <param name="AFileName">Имя файла(можо не указавать)</param>
    /// <param name="AVisible">видемость файла. True - видить, False - нет</param>
    /// <param name="AQuit">Закрывать Excel, после финализации объекта.</param>
    procedure Open(const AFileName: String; const AVisible: Boolean; const AQuit: Boolean); overload;
    // ------------------------------------------------------------------------
    procedure Save(const AFileName: String);
    property WorkSheets: TWorkSheetList read FWorkSheets;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.Win.ComObj,
  Lb.Logger,
  Lb.Temple.SysUtils;

{ TWorkSheet }

constructor TWorkSheet.Create(const AWorkBook: TWorkBook; const AWorkSheet: OleVariant);
begin
  FWorkBook := AWorkBook;
  FWorkSheet := AWorkSheet;
  Self.SetInitialization;
  Self.SetCommands;
end;

destructor TWorkSheet.Destroy;
begin
  FValue := Unassigned;
  FWorkSheet := Unassigned;
  inherited;
end;

procedure TWorkSheet.SetInitialization;
begin
  FRowCount := FWorkSheet.UsedRange.Rows.Count;
  FColCount := FWorkSheet.UsedRange.Columns.Count;
  FValue := FWorkSheet.UsedRange.Value;
  FName := FWorkSheet.Name;
end;

procedure TWorkSheet.SetCommands;
{$IFDEF COMMANDS_V2}
begin

end;
{$ELSE}
  function GetStringReplace(const Source, OldPattern, NewPattern: String): String;
  var
    xStrBegin, xStrEnd: String;
    xIndBegin, xIndEnd: Integer;
  begin
    // ddd {name1:param1:param2:param3} ddd {name2:param1:param2:param3}
    Result := '';
    xIndBegin := Pos('{' + OldPattern,Source);
    if xIndBegin > 0 then
    begin
      xIndEnd := Pos('}',Source,xIndBegin);
      xStrBegin := Copy(Source,1,xIndBegin - 1);
      xStrEnd := Copy(Source,xIndEnd + 1,Source.Length - xIndEnd);
      Result := xStrBegin + NewPattern + xStrEnd;
    end;
  end;

  procedure SetCommandExe(const ACol, ARow: Integer; AValue: String);
  var
    xIndParam: Integer;
    xHieghtParam: Integer;
    xHieghtExcel: Integer;
    xCountRowParam: Integer;
  var
    xInd: Integer;
    xS, xValue, xName: String;
    i, Count: Integer;
    xObjectCommand: TObjectCommand;
    xObjCmds: TObjectCommandList;
  var
    xVal: TValueString;
  begin
    try
      xObjCmds := TObjectCommandList.Create;
      xS := AValue;
      SetObjectCommands(xS,xObjCmds);
      Count := xObjCmds.Count;
      if Count > 0 then
      begin
        {$IFDEF SET_COMMAND_EXE}
        TLogger.LogForm('commad','Команды Excel');
        TLogger.LogForm('commad','  >> col := ' +  IntToStr(ACol));
        TLogger.LogForm('commad','  >> row := ' +  IntToStr(ARow));
        TLogger.LogForm('commad','  >> value := ' +  AValue);
        TLogger.LogForm('commad','  >> количестов count ' +  IntToStr(Count));
        {$ENDIF}
        for i := 0 to Count - 1 do
        begin
          xObjectCommand := xObjCmds[i];
          xName := xObjectCommand.Name;
          xInd := FWorkBook.Commands.IndexOf(xName);
          {todo: Выводить список}
          if xInd >= 0 then
          begin
            xValue := FWorkBook.Commands.Items[xInd].Value;
            {$IFDEF SET_COMMAND_EXE}
            TLogger.LogForm('commad','  >> name := ' + xName);
            TLogger.LogForm('commad','  >> value := ' + xValue);
            {$ENDIF}
            xS := GetStringReplace(xS,xObjectCommand.Name,xValue);
            // -------------------------------------------------
            {$IFDEF SET_COMMAND_EXE}
            TLogger.LogForm('commad','  >> result_value := ' + xS);
            {$ENDIF}
            // -------------------------------------------------
            // Отработка параметров
            // {___:height:110}
            xIndParam := xObjectCommand.Params.IndexOf('height');
            if xIndParam >= 0 then
            begin
              xHieghtParam := StrToIntDef(xObjectCommand.Params[xIndParam + 1],0);
              {$IFDEF SET_COMMAND_EXE}
              TLogger.LogForm('commad','  >> hieght_param := ' + IntToStr(xHieghtParam));
              {$ENDIF}

              xVal := TValueString.Create;
              try
                xVal.Value := xS;

                xHieghtExcel := Self.RowHieght[ARow];
                xCountRowParam := xVal.CountResult;
                if xCountRowParam > 0 then
                  if xHieghtExcel < (xCountRowParam * 15) then
                  begin
                    Self.RowHieght[ARow] := xCountRowParam * 15;
                  end;

              finally
                FreeAndNil(xVal);
              end;


              xCountRowParam := Trunc(xS.Length/xHieghtParam);
              if xCountRowParam = 0 then xCountRowParam := 1;

              xHieghtExcel := Self.RowHieght[ARow];
              if xHieghtExcel < (xCountRowParam * 15) then
              begin
                Self.RowHieght[ARow] := xCountRowParam * 15;
              end;


            end;
            // -------------------------------------------------
          end;
        end;
        FValue[ARow,ACol] := xS;
      end;
      // -------------------------------------------------------
      if (Pos('{',xS) > 0) and (Pos('}',xS) > 0) then
      begin

            if ARow = 40 then
            begin
              with TStringList.Create do
              begin
                Free;
              end;
            end;

        FValue[ARow,ACol] := '';
        SetRowMinHieght(ARow);
      end;
      // --------------------------------------------------------
    finally
      xObjCmds.Clear;
      FreeAndNil(xObjCmds);
    end;
  end;

var
  i, j: Integer;
  xS: String;
begin
  // Процедура исполняет
  if not Assigned(FWorkBook) then Exit;
  if not Assigned(FWorkBook.Commands) then Exit;

  // перебираем все ячейки по поиску коменды
  if (FRowCount > 0) and (FColCount > 0) then
    for i := 1 to FRowCount do
      for j := 1 to FColCount do
      begin
        xS := VarToStrDef(FValue[i,j],'');
        {todo: Прочитать команду, параметры строки, что нужно сделать}
        if xS.IsEmpty then Continue;
        SetCommandExe(j,i,xS);
      end;
  FWorkSheet.UsedRange.Value := FValue;
end;
{$ENDIF}

function TWorkSheet.ToString: string;
var
  xS: String;
begin
  xS := inherited ToString;
  xS := xS + '[' + FName + '] row_count = ' + IntToStr(FRowCount) + ' col_count = ' + IntToStr(FColCount);
  Result := xS;
end;

function TWorkSheet.GetRowHieght(Row: Integer): Integer;
var
  xRow, xR: OleVariant;
begin
  xRow :=  FWorksheet.Rows;
  xR := xRow.Rows[Row];
  Result := xR.RowHeight;
end;

procedure TWorkSheet.SetRowHeight(Row: Integer; const Value: Integer);
var
  xRow, xR: OleVariant;
begin
  try
    xRow := FWorksheet.Rows;
    xR := xRow.Rows[Row];
    xR.RowHeight := Value;
  except
  end;
end;


procedure TWorkSheet.SetRowMinHieght(Row: Integer);
var
  i: Integer;
  xB: Boolean;
  xS: String;
begin
  xB := False;
  for i := 1 to FColCount do
  begin
    xS := VarToStrDef(FValue[Row,i],'');
    if not xB then xB := not xS.IsEmpty;
  end;
  if not xB then
    RowHieght[Row] := 0;
end;

{ TWorkBook }

constructor TWorkBook.Create;
begin
  FAppExcelVisible := False;
  FAppExcelQuit := False;
  FCommands := TCommands.Create;
  FAppProgID := 'Excel.Application';
  FWorkSheets:= TWorkSheetList.Create;
end;

destructor TWorkBook.Destroy;
begin
  FWorkbook := Unassigned;
  if FAppExcelQuit then FAppExcel.Quit;
  FAppExcel := Unassigned;
  FreeAndNil(FWorkSheets);
  FreeAndNil(FCommands);
  inherited;
end;

procedure TWorkBook.Save(const AFileName: String);
begin
  try
    FWorkbook.SaveAs(AFileName);
  except on E: Exception do
    raise Exception.Create('Error Message: ' + E.Message);
  end;
end;

procedure TWorkBook.SetInitializationExcel;
var
  Result: HRESULT;
  Unknown: IUnknown;
begin
  FWorkSheets.Clear;
  Result := GetActiveObject(ProgIDToClassID(FAppProgID),nil,Unknown);
  if (Result = MK_E_UNAVAILABLE) then
    FAppExcel := CreateOleObject(FAppProgID)
  else
  begin
    FAppExcel := GetActiveOleObject(FAppProgID);
  end;
  FAppExcel.Application.EnableEvents := False;
  FAppExcel.DisplayAlerts := False;
  FAppExcel.SheetsInNewWorkbook := 1;
  FAppExcel.Visible := FAppExcelVisible;

  if FFileName.IsEmpty then
    FWorkbook := FAppExcel.Workbooks.Add
  else
    FWorkbook := FAppExcel.Workbooks.Add(FFileName);
  SetWorkSheet(FWorkbook.Sheets);
end;

procedure TWorkBook.Open;
begin
  Self.Open('',False,True);
end;

procedure TWorkBook.Open(const AFileName: String);
begin
  Self.Open(AFileName,False,True);
end;

procedure TWorkBook.Open(const AFileName: String; AVisible: Boolean);
begin
  Self.Open(AFileName,AVisible,True);
end;

procedure TWorkBook.Open(const AFileName: String; const AVisible: Boolean; const AQuit: Boolean);
begin
  FAppExcelVisible := AVisible;
  FAppExcelQuit := AQuit;
  FFileName := AFileName;
  Self.SetInitializationExcel;
end;

procedure TWorkBook.SetWorkSheet(AWorkSheets: OleVariant);
var
  i, iCount: Integer;
begin
  iCount := AWorkSheets.Count;
  if iCount > 0 then
    for i := 1 to iCount do
     FWorkSheets.Add(TWorkSheet.Create(Self,AWorkSheets.Item[i]))
  else
    raise Exception.Create('Error Message: Нет строницы');
end;

end.
