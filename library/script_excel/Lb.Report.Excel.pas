(*******************************************************************************
  Обертка для работы с Exele

  https://club.directum.ru/post/773
*******************************************************************************)
unit Lb.Report.Excel;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes;

type
  TReportExcel = class(TObject)
  public type

    /// <summary>
    /// Константы для горизонтальной настройки
    /// </summary>
    TxlHAlign = record
    const
      xlHAlignCenter = -4108; // Центр.
      xlHAlignCenterAcrossSelection = 7;// Центр по выбору.
      xlHAlignDistributed = -4117; // Распределить.
      xlHAlignFill = 5; // Заполните.
      xlHAlignGeneral = 1; // Выровнять по типу данных.
      xlHAlignJustify = -4130; // Обоснуйте.
      xlHAlignLeft = -4131; // Оставил.
      xlHAlignRight = -4152; // Правильно.
    end;

    /// <summary>
    /// Константы для вертикальной настройки
    /// </summary>
    TxlVAlign = record
      const
        xlVAlignBottom = -4107; // Bottom
        xlVAlignCenter = -4108; // Center
        xlVAlignDistributed = -4117; // Distributed
        xlVAlignJustify = -4130; // Justify
        xlVAlignTop = -4160; // Top
    end;

    /// <summary>
    /// Задает Вес границы вокруг диапазона.
    /// </summary>
    TxlBorderWeight = record
      const
        xlHairline = 1;    // Сверхтонкая (тонкой границы).
        xlMedium = -4138;  // От среднего.
        xlThick =	4;       // Жирное (широкой границы).
        xlThin = 2;        // Тонкие.
    end;

  public const {напровление линии, индексации лини}
    xlContinuous = $00000001;
    xlInsideHorizontal = $0000000C;
    xlInsideVertical = $0000000B;
    xlDiagonalDown = $00000005;
    xlDiagonalUp = $00000006;
    xlEdgeBottom = $00000009;
    xlEdgeLeft = $00000007;
    xlEdgeRight = $0000000A;
    xlEdgeTop = $00000008;
  private
    FAppProgID: String;
    FAppExcel: OleVariant;
    FWorkbook: OleVariant;
    FWorkSheet: OleVariant;
  private
    FFileName: String;
  protected
    // procedure SetFrameBorder(ABorder: OleVariant; AWeight: Integer = TxlBorderWeight.xlThin);
    // procedure SetFrameBorders(ABorders: OleVariant; AWeight: Integer = TxlBorderWeight.xlThin);
    procedure SetInitializationExcel;
  public
    constructor Create(const AFileName: String = ''); virtual;
    destructor Destroy; override;
    /// <summary>
    /// Установить размер колонки
    /// </summary>
    procedure SetColumnWidth(const ACol, AWidth: Double);

    /// <summary>
    /// Установить высоту строки
    /// </summary>
    procedure SetRowHeight(const ARow: Integer; const AHeight: Double);

    /// <summary>
    /// Записать данные в ячейку
    /// </summary>
    procedure SetCellsValue(ACol, ARow: Integer; AValue: String);

    procedure SetCellsValueText(ACol, ARow: Integer; AValue: String);

    /// <summary>
    /// Объединение не скольких ячеик
    /// </summary>
    /// <remarks>
    ///  <para>Укзываются две кранийх ячейки,</para>
    ///  <para>ACol1, ARow1 - первая ячека</para>
    ///  <para>ACol2, ARow2 - второая ячейка</para>
    ///  <para>AMergeCells - включает объединять или нет</para>
    /// </remarks>
    procedure SetCellsMerge(const ACol1, ARow1, ACol2, ARow2: Integer; AMergeCells: Boolean = True);
    /// <summary>
    /// Вертикальное выравневание
    /// </summary>
    procedure SetVerticalAlignment(const ACol1, ARow1, ACol2, ARow2, AVerticalAlignment: Integer);
    /// <summary>
    /// Горизонтальное выраневание
    /// </summary>
    procedure SetHorizontalAlignment(const ACol1, ARow1, ACol2, ARow2, AHorizontalAlignment: Integer);
    /// <summary>
    /// Зафиксировать количество строк
    /// </summary>
    procedure SetFreezePanesRow(const ASplitRow: Integer);
  public {Вывод рамки и линиий}
    /// <summary>
    /// Поставить рамку, на чейку
    /// </summary>
    procedure SetFrameCell(const ACol, ARow: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
    /// <summary>
    /// Рамка на область
    /// </summary>
    procedure SetFrameRangeCell(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
    procedure SetFrameDiagonalDown(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
    procedure SetFrameDiagonalUp(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
    procedure SetFrameLineStyle(const ACol1, ARow1, ACol2, ARow2: Integer; const AIndexLine: Integer = xlDiagonalUp; const AWeight: Integer = TxlBorderWeight.xlThin);
    /// <summary>
    /// Поверкуть текст под углом
    /// </summary>
    procedure SetCelleOrientation(const ACol1, ARow1, ACol2, ARow2: Integer; AOrientation: Integer);
  public {Font}
    /// <summary>
    /// Переносить текст
    /// </summary>
    procedure SetCellsWrapText(ACol, ARow: Integer; AValue: Boolean = True);
    procedure SetCellsFrameWrapText(ACol1, ARow1, ACol2, ARow2: Integer; AValue: Boolean = True);
    /// <summary>
    /// Жирный шрифт или нет
    /// </summary>
    procedure SetCellsFontBold(ACol1, ARow1, ACol2, ARow2: Integer; AValue: Boolean);
    procedure SetCellsFontItalic(ACol1, ARow1, ACol2, ARow2: Integer; AValue: Boolean);
    /// <summary>
    /// Установка темы шрифта
    /// </summary>
    procedure SetCellsFontName(ACol1, ARow1, ACol2, ARow2: Integer; AName: String = 'Times New Roman');
    /// <summary>
    /// Установка размер шрифта
    /// </summary>
    procedure SetCellsFontSize(ACol1, ARow1, ACol2, ARow2: Integer; ASize: Integer = 11);
  public
    /// <summary>
    /// Показать с формированную таблицу
    /// </summary>
    procedure SetPost;
  end;

/// <summary>
/// Текст выводится в рамках, по вертикале и горизонтале
/// </summary>
procedure SetReportExcelTitle(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String);
procedure SetReportExcelTitleFrame(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String); overload;
procedure SetReportExcelTitleFrame(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String; const AWidth, Angle: Integer); overload;

/// <summary>
/// Тект Выводится по ценрту по вертикале и горизонтале
/// </summary>
procedure SetCellHeaderExcel(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
/// <summary>
/// Тект Выводится по ценрту по вертикале и горизонтале, без рамке
/// </summary>
procedure SetCellHeaderExcelNonFrame(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);

/// <summary>
/// Текст выводится, в рамке и в пределы ячейки
/// </summary>
procedure SetCellExcel(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
/// <summary>
/// Текст выводится, без рамке и в пределы ячейки
/// </summary>
procedure SetCellExcelNonFrame(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);

/// <summary>
/// Текст выводить, текст пределых [Col1, Row1, Col2, Row2] без рамки, объединением
/// </summary>
procedure SetCellExcelMerge(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String);

procedure SetColumnWidth(const AReportExcel: TReportExcel; const ACol: Integer; const AWidth: Double);

procedure SetRowHeight(const AReportExcel: TReportExcel; const ARow: Integer; const AHeight: Double);

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.Win.ComObj;

procedure SetReportExcelTitle(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValue(ACol1,ARow1,AValue);
    AReportExcel.SetCellsMerge(ACol1,ARow1,ACol2,ARow2);
    AReportExcel.SetVerticalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlHAlign.xlHAlignCenter);
  end;
end;

procedure SetReportExcelTitleFrame(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String; const AWidth, Angle: Integer);
begin
  if Assigned(AReportExcel) then
  begin
    // AReportExcel.SetCellsValue(ACol1,ARow1,AValue);
    AReportExcel.SetCellsValueText(ACol1,ARow1,AValue);
    AReportExcel.SetCellsWrapText(ACol1,ARow1);
    AReportExcel.SetCellsFontBold(ACol1,ARow1,ACol2,ARow2,True);
    AReportExcel.SetCellsMerge(ACol1,ARow1,ACol2,ARow2);
    AReportExcel.SetVerticalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlHAlign.xlHAlignCenter);
    AReportExcel.SetCelleOrientation(ACol1, ARow1, ACol2, ARow2,Angle);
    AReportExcel.SetFrameRangeCell(ACol1,ARow1,ACol2,ARow2,AWidth);
  end;
end;

procedure SetReportExcelTitleFrame(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValue(ACol1,ARow1,AValue);
    AReportExcel.SetCellsMerge(ACol1,ARow1,ACol2,ARow2);
    AReportExcel.SetVerticalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlHAlign.xlHAlignLeft);
    AReportExcel.SetFrameRangeCell(ACol1,ARow1,ACol2,ARow2);
  end;
end;


procedure SetCellHeaderExcel(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValueText(ACol,ARow,AValue);
    AReportExcel.SetFrameCell(ACol,ARow);
    AReportExcel.SetCellsWrapText(ACol,ARow,True);
    AReportExcel.SetVerticalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlHAlign.xlHAlignCenter);
  end;
end;

procedure SetCellHeaderExcelNonFrame(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValueText(ACol,ARow,AValue);
    // AReportExcel.SetFrameCell(ACol,ARow);
    AReportExcel.SetCellsWrapText(ACol,ARow,True);
    AReportExcel.SetVerticalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlHAlign.xlHAlignCenter);
  end;
end;

procedure SetCellExcel(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValue(ACol,ARow,AValue);
    AReportExcel.SetFrameCell(ACol,ARow);
    AReportExcel.SetCellsWrapText(ACol,ARow,True);
    AReportExcel.SetVerticalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlHAlign.xlHAlignLeft);
  end;
end;

procedure SetCellExcelNonFrame(const AReportExcel: TReportExcel; const ACol, ARow: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValue(ACol,ARow,AValue);
    // AReportExcel.SetFrameCell(ACol,ARow);
    AReportExcel.SetCellsWrapText(ACol,ARow,True);
    AReportExcel.SetVerticalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol,ARow,ACol,ARow,TReportExcel.TxlHAlign.xlHAlignLeft);
  end;
end;

procedure SetCellExcelMerge(const AReportExcel: TReportExcel; const ACol1, ARow1, ACol2, ARow2: Integer; const AValue: String);
begin
  if Assigned(AReportExcel) then
  begin
    AReportExcel.SetCellsValue(ACol1,ARow1,AValue);
    AReportExcel.SetCellsWrapText(ACol1,ARow1,True);
    AReportExcel.SetCellsMerge(ACol1, ARow1, ACol2, ARow2);
    AReportExcel.SetVerticalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlVAlign.xlVAlignCenter);
    AReportExcel.SetHorizontalAlignment(ACol1,ARow1,ACol2,ARow2,TReportExcel.TxlHAlign.xlHAlignLeft);
  end;
end;

procedure SetColumnWidth(const AReportExcel: TReportExcel; const ACol: Integer; const AWidth: Double);
begin
  if Assigned(AReportExcel) then
    AReportExcel.SetColumnWidth(ACol,AWidth);
end;

procedure SetRowHeight(const AReportExcel: TReportExcel; const ARow: Integer; const AHeight: Double);
begin
  if Assigned(AReportExcel) then
    AReportExcel.SetRowHeight(ARow,AHeight);
end;


{ TReportExcel }

constructor TReportExcel.Create(const AFileName: String = '');
begin
  FFileName := AFileName;
  FAppProgID := 'Excel.Application';
  SetInitializationExcel;
end;

destructor TReportExcel.Destroy;
begin
  FWorkSheet := Unassigned;
  FWorkbook := Unassigned;
  FAppExcel := Unassigned;
  inherited;
end;


procedure TReportExcel.SetInitializationExcel;
var
  Result: HRESULT;
  Unknown: IUnknown;
begin
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

  if FFileName.IsEmpty then
  begin
    FWorkbook := FAppExcel.Workbooks.Add;
    FWorkSheet := FWorkbook.Sheets[1];
  end
  else
  begin
    FWorkbook := FAppExcel.Workbooks.Add(FFileName);
    FWorkSheet := FWorkbook.Sheets[1];
  end;
  //
  // FAppExcel.Visible := True;
  // FAppExcel := Unassigned;
end;


procedure TReportExcel.SetPost;
begin
  FAppExcel.Visible := True;
end;

procedure TReportExcel.SetColumnWidth(const ACol, AWidth: Double);
var
  Colum, xC: OleVariant;
begin
  Colum := FWorksheet.Columns;
  xC := Colum.Columns[ACol];
  xC.ColumnWidth := AWidth;
end;

procedure TReportExcel.SetRowHeight(const ARow: Integer; const AHeight: Double);
var
  Row, xR: OleVariant;
begin
  Row :=  FWorksheet.Rows;
  xR := Row.Rows[ARow];
  xR.RowHeight := AHeight;
end;

//procedure TReportExcel.SetFrameBorder(ABorder: OleVariant; AWeight: Integer = TxlBorderWeight.xlThin);
//begin
//  ABorder.LineStyle := xlContinuous;
//  ABorder.Weight := AWeight;
//end;

//procedure TReportExcel.SetFrameBorders(ABorders: OleVariant; AWeight: Integer = TxlBorderWeight.xlThin);
//begin
//  SetFrameBorder(ABorders[xlEdgeBottom],AWeight);
//  SetFrameBorder(ABorders[xlEdgeLeft],AWeight);
//  SetFrameBorder(ABorders[xlEdgeRight],AWeight);
//  SetFrameBorder(ABorders[xlEdgeTop],AWeight);
//end;

procedure TReportExcel.SetFrameCell(const ACol, ARow: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
var
  x: OleVariant;
begin
  x := FWorkSheet.Cells[ARow,ACol];
  x.Borders[xlEdgeBottom].LineStyle := xlContinuous;
  x.Borders[xlEdgeLeft].LineStyle := xlContinuous;      // - слева нет
  x.Borders[xlEdgeRight].LineStyle := xlContinuous;     // - справа нет
  x.Borders[xlEdgeTop].LineStyle := xlContinuous;       // - сверху нетs
  // FWorkSheet.Cells[ARow,ACol].Borders[xlEdgeBottom].LineStyle := xlContinuous;
  // FWorkSheet.Cells[ARow,ACol].Borders[xlEdgeLeft].LineStyle := xlContinuous;      // - слева нет
  // FWorkSheet.Cells[ARow,ACol].Borders[xlEdgeRight].LineStyle := xlContinuous;     // - справа нет
  // FWorkSheet.Cells[ARow,ACol].Borders[xlEdgeTop].LineStyle := xlContinuous;       // - сверху нетs
end;

procedure TReportExcel.SetFrameRangeCell(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];

  xM.Borders[xlEdgeBottom].LineStyle := xlContinuous;
  xM.Borders[xlEdgeBottom].Weight := AWeight;

  xM.Borders[xlEdgeLeft].LineStyle := xlContinuous;      // - слева нет
  xM.Borders[xlEdgeLeft].Weight := AWeight;

  xM.Borders[xlEdgeRight].LineStyle := xlContinuous;     // - справа нет
  xM.Borders[xlEdgeRight].Weight := AWeight;

  xM.Borders[xlEdgeTop].LineStyle := xlContinuous;       // - сверху нетs
  xM.Borders[xlEdgeTop].Weight := AWeight;
end;


procedure TReportExcel.SetFreezePanesRow(const ASplitRow: Integer);
begin
  FWorkSheet.Application.ActiveWindow.SplitRow := ASplitRow;
  FWorkSheet.Application.ActiveWindow.FreezePanes := True;
end;

procedure TReportExcel.SetFrameDiagonalDown(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Borders[xlDiagonalDown].LineStyle := xlContinuous;
end;

procedure TReportExcel.SetFrameDiagonalUp(const ACol1, ARow1, ACol2, ARow2: Integer; AWeight: Integer = TxlBorderWeight.xlThin);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Borders[xlDiagonalUp].LineStyle := xlContinuous;
end;

procedure TReportExcel.SetFrameLineStyle(const ACol1, ARow1, ACol2, ARow2: Integer; const AIndexLine: Integer = xlDiagonalUp; const AWeight: Integer = TxlBorderWeight.xlThin);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Borders[AIndexLine].LineStyle := xlContinuous;
  xM.Borders[AIndexLine].Weight := AWeight;
end;

procedure TReportExcel.SetCellsFontBold(ACol1, ARow1, ACol2, ARow2: Integer; AValue: Boolean);
var
  x1, x2, xM: OleVariant;
begin
  {todo: Нужно будет передать функцию для вывода разными стелями текст}
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Font.Bold := AValue;
end;

procedure TReportExcel.SetCellsFontItalic(ACol1, ARow1, ACol2, ARow2: Integer;
  AValue: Boolean);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Font.Italic := AValue;
end;

procedure TReportExcel.SetCellsFontName(ACol1, ARow1, ACol2, ARow2: Integer;
  AName: String);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Font.Name := AName;
end;

procedure TReportExcel.SetCellsFontSize(ACol1, ARow1, ACol2, ARow2,
  ASize: Integer);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.Font.Size := ASize;
end;

procedure TReportExcel.SetCellsValue(ACol, ARow: Integer; AValue: String);
var
  xVal: OleVariant;
begin
  xVal := FWorkSheet.Cells[ARow,ACol];
  xVal.Value := AValue;
end;


procedure TReportExcel.SetCellsValueText(ACol, ARow: Integer; AValue: String);
var
  xVal: OleVariant;
begin
  xVal := FWorkSheet.Cells[ARow,ACol];
  //xVal.Characters;
  xVal.NumberFormat := '@';
  xVal.Value := AValue;
end;

procedure TReportExcel.SetCellsWrapText(ACol, ARow: Integer; AValue: Boolean);
var
  xVal: OleVariant;
begin
  xVal := FWorkSheet.Cells[ARow,ACol];
  xVal.WrapText:= AValue;
end;

procedure TReportExcel.SetCellsFrameWrapText(ACol1, ARow1, ACol2, ARow2: Integer; AValue: Boolean);
var
  // xVal: OleVariant;
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.WrapText := AValue;
end;


procedure TReportExcel.SetCellsMerge(const ACol1, ARow1, ACol2, ARow2: Integer;
  AMergeCells: Boolean);
var
  x1, x2, xM: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xM := FWorkSheet.Range[x1,x2];
  xM.MergeCells := AMergeCells;
end;



procedure TReportExcel.SetVerticalAlignment(const ACol1, ARow1, ACol2, ARow2, AVerticalAlignment: Integer);
var
  x1, x2, xV: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xV := FWorkSheet.Range[x1,x2];
  xV.VerticalAlignment := AVerticalAlignment;
end;

procedure TReportExcel.SetHorizontalAlignment(const ACol1, ARow1, ACol2, ARow2,
  AHorizontalAlignment: Integer);
var
  x1, x2, xH: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xH := FWorkSheet.Range[x1,x2];
  xH.HorizontalAlignment := AHorizontalAlignment;
end;

procedure TReportExcel.SetCelleOrientation(const ACol1, ARow1, ACol2,
  ARow2: Integer; AOrientation: Integer);
var
  x1, x2, xH: OleVariant;
begin
  x1 := FWorkSheet.Cells[ARow1,ACol1];
  x2 := FWorkSheet.Cells[ARow2,ACol2];
  xH := FWorkSheet.Range[x1,x2];
  xH.Orientation := AOrientation;
end;

end.
