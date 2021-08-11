unit Lb.Temple.Excel;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

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
    FWorkSheets: TWorkSheetList;
  protected
    procedure SetWorkSheet(AWorkSheets: OleVariant);
    procedure SetInitializationExcel;
  public
    constructor Create; virtual;
    destructor Destroy; override;
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
  Vcl.Clipbrd,
  System.Win.ComObj,
  Lb.Logger,
  Lb.Temple.SysUtils;

{ TWorkSheet }

constructor TWorkSheet.Create(const AWorkBook: TWorkBook; const AWorkSheet: OleVariant);
begin
  FWorkBook := AWorkBook;
  FWorkSheet := AWorkSheet;
  Self.SetInitialization;
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
  FAppProgID := 'Excel.Application';
  FWorkSheets:= TWorkSheetList.Create;
end;

destructor TWorkBook.Destroy;
begin
  FWorkbook := Unassigned;
  if FAppExcelQuit then FAppExcel.Quit;
  FAppExcel := Unassigned;
  FreeAndNil(FWorkSheets);
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
