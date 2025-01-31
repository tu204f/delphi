unit UnitMainForm;

interface

{$I test.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.ImgList, System.Actions, Vcl.ActnList, UnitListFrame, UnitTreeListFrame,
  Vcl.StdCtrls, Vcl.Menus,
  Lb.Test.SysUtils;

type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    GridPanel: TGridPanel;
    ImageList: TImageList;
    ActionList: TActionList;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    TreeListFrame: TTreeListFrame;
    ListButtonedEdit: TButtonedEdit;
    ListBox: TListBox;
    TreeButtonedEdit: TButtonedEdit;
    TreeView: TTreeView;
    OpenDialog: TOpenDialog;
    ActionLoadList: TAction;
    ActionAddNode: TAction;
    ActionDeleteNode: TAction;
    PopupMenu: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ActionShLeft: TAction;
    ActionShRight: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ActionLoadTree: TAction;
    ActionSaveTree: TAction;
    ToolButton11: TToolButton;
    SaveDialog: TSaveDialog;
    Panel1: TPanel;
    Label1: TLabel;
    EditName: TEdit;
    EditYear: TEdit;
    EditAuthor: TEdit;
    EditPath: TEdit;
    CheckBoxParser: TCheckBox;
    RadioButtonAuthor: TRadioButton;
    RadioButtonYear: TRadioButton;
    RadioButtonPath: TRadioButton;
    procedure ListButtonedEditRightButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionLoadListExecute(Sender: TObject);
    procedure ActionAddNodeExecute(Sender: TObject);
    procedure ActionDeleteNodeExecute(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionShLeftExecute(Sender: TObject);
    procedure ActionShRightExecute(Sender: TObject);
    procedure ActionLoadTreeExecute(Sender: TObject);
    procedure ActionSaveTreeExecute(Sender: TObject);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeButtonedEditRightButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure CheckBoxParserClick(Sender: TObject);
  private
    SelectedTreeNode: TTreeNode;
    function SetShRight(const AValue: String): Boolean;

    procedure SetInfoFilmParserClear;
    function GetInfoFilmParser(const AValue: String): TInfoFilmParser;
    function FindTreeNode(ATreeNode: TTreeNode; AName: String): TTreeNode;
  protected
    function GetPathTree(ATreeNode: TTreeNode): String;
    procedure SetStatusEvent(S: String);
  public
    ///<summary>
    /// Инициализация входных параметров
    ///</summary>
    procedure SetInitInputParam;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.ApplicationVersion,
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  Lb.Setting;

procedure TMainForm.FormShow(Sender: TObject);
var
  xCaption: String;
begin
  xCaption := Format('Тестовое задание для AltaSoft [%s]',[GetApplicationVersion]);
  {$IFDEF DEBUG}
  xCaption := xCaption + ' debug';
  {$ENDIF}
  Self.Caption := xCaption;
  SetInitInputParam;
end;


function TMainForm.GetInfoFilmParser(const AValue: String): TInfoFilmParser;
var
  xInfoFilmParser: TInfoFilmParser;
begin
  xInfoFilmParser.SetText(AValue);
  EditName.Text   := xInfoFilmParser.Name;
  EditYear.Text   := xInfoFilmParser.Year;
  EditAuthor.Text := xInfoFilmParser.Author;
  EditPath.Text   := xInfoFilmParser.Path;
  Result := xInfoFilmParser;
end;

procedure TMainForm.SetInfoFilmParserClear;
begin
  EditName.Text   := '';
  EditYear.Text   := '';
  EditAuthor.Text := '';
  EditPath.Text   := '';
end;

procedure TMainForm.SetInitInputParam;
begin
  SelectedTreeNode := nil;

  OpenDialog.InitialDir := GetApplicationPath;
  SaveDialog.InitialDir := GetApplicationPath;

  ListButtonedEdit.Text  := TSetting.ReadString('config.sys.list_filename','');
  TreeButtonedEdit.Text  := TSetting.ReadString('config.sys.tree_list_filename','');
  CheckBoxParser.Checked := TSetting.ReadBool('config.sys.is_parser',False);

  // Загрузка по умолчанию
  if SetLoadList(ListButtonedEdit.Text,ListBox.Items) then
    ListBox.SetFocus;

  SetInfoFilmParserClear;
end;

function TMainForm.GetPathTree(ATreeNode: TTreeNode): String;
var
  xS: String;
  xTreeNode: TTreeNode;
begin
  xS := '';
  xTreeNode := ATreeNode;
  while Assigned(xTreeNode) do
  begin
    case Integer(xTreeNode.Data) of
      TREE_NODE_DIR: xS := Trim(xTreeNode.Text) + PathDelim + xS;
      TREE_NODE_VALUE: xS := Trim(xTreeNode.Text);
    else
      xS := Trim(xTreeNode.Text) + PathDelim + xS;
    end;
    xTreeNode := xTreeNode.Parent;
  end;
  Result := xS;
end;


procedure TMainForm.SetStatusEvent(S: String);

  function _GetText(S: String): String;
  var
    xS: String;
    xC: Char;
    i, L: Integer;
  begin
    L := Length(S);
    if L > 0 then
      for i := 1 to L do
      begin
        xC := S[i];
        if CharInSet(xC,[#9,#13,#10]) then
          xS := xS + ' '
        else
          xS := xS + xC;
      end;
    Result := xS;
  end;

begin
  StatusBar.SimpleText := _GetText(S);
end;

procedure TMainForm.ActionLoadListExecute(Sender: TObject);
var
  xFileName: String;
begin
  if OpenDialog.Execute then
  begin
    xFileName := OpenDialog.FileName;
    if SetLoadList(xFileName,ListBox.Items) then
    begin
      TSetting.WriteString('config.sys.list_filename',xFileName);
      ListButtonedEdit.Text := xFileName;
      ListBox.SetFocus;
    end;
  end;
end;

function TMainForm.FindTreeNode(ATreeNode: TTreeNode; AName: String): TTreeNode;
var
  i: Integer;
  xTreeNode: TTreeNode;
begin
  Result := nil;
  if Assigned(ATreeNode) then
  begin
    for i := 0 to ATreeNode.Count - 1 do
    begin
      xTreeNode := ATreeNode.Item[i];
      if SameText(xTreeNode.Text,AName) then
      begin
        Result := xTreeNode;
        Break;
      end;
    end;
  end else
  begin
    for xTreeNode in TreeView.Items do
      if SameText(xTreeNode.Text,AName) then
      begin
        Result := xTreeNode;
        Break;
      end;
  end;
end;

procedure TMainForm.ActionLoadTreeExecute(Sender: TObject);

  procedure _DeleteListBox(S: String);
  var
    xIndex: Integer;
  begin
    // Удаляем дубликаты с левой стороны
    xIndex := ListBox.Items.IndexOf(S);
    if xIndex >= 0 then
      ListBox.Items.Delete(xIndex);
  end;

  procedure _SetLine(const AValue: String);
  var
    xS: String;
    i: Integer;
    xSource: TStringList;
    xTreeNode, xTreeNodeDir: TTreeNode;
  begin
    xSource := TStringList.Create;
    try
      xTreeNode := nil;
      xTreeNodeDir := nil;

      SetParserLine(xSource, AValue);

      for i := 0 to xSource.Count - 1 do
      begin
        xS := xSource[i];
        xTreeNode := Self.FindTreeNode(xTreeNode,xS);
        if i = (xSource.Count - 1) then
        begin
          xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,xS);
          xTreeNode.Data := Pointer(TREE_NODE_VALUE);
          xTreeNode.ImageIndex := 5;
          xTreeNode.SelectedIndex := 5;
          TreeView.Selected := xTreeNode;
          xTreeNode.Expand(False);

          _DeleteListBox(xS);
        end else
        begin
          if Assigned(xTreeNode) then
          begin
            xTreeNodeDir := xTreeNode;
          end else
          begin
            xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,xS);
            xTreeNode.Data := Pointer(TREE_NODE_DIR);
            xTreeNode.ImageIndex := 0;
            TreeView.Selected := xTreeNode;
            xTreeNode.Expand(False);
            xTreeNodeDir := xTreeNode;
          end;
        end;
      end;

    finally
      FreeAndNil(xSource);
    end;
  end;

var
  xS: String;
  xStr: TStrings;
  xFileName: String;
begin
  if OpenDialog.Execute then
  begin
    xFileName := OpenDialog.FileName;
    TreeButtonedEdit.Text := xFileName;
    TSetting.WriteString('config.sys.tree_list_filename',xFileName);
    xStr := TStringList.Create;
    try
      TreeView.Items.Clear;
      xStr.LoadFromFile(xFileName);
      for xS in xStr do
        _SetLine(xS);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

procedure TMainForm.ActionSaveTreeExecute(Sender: TObject);

  procedure _SetSave(ASource: TStrings);
  var
    xS: String;
    xTreeNode: TTreeNode;
  begin
    ASource.Clear;
    xTreeNode := TreeView.Items.GetFirstNode;
    repeat
      case Integer(xTreeNode.Data) of
        TREE_NODE_DIR: ;
        TREE_NODE_VALUE: begin
          xS := PathDelim + GetPathTree(xTreeNode);
          ASource.Add(xS);
        end;
      end;
      xTreeNode := xTreeNode.GetNext;
    until not Assigned(xTreeNode);
  end;

var
  xStr: TStrings;
  xFileName: String;
begin
  if SaveDialog.Execute then
  begin
    xFileName := SaveDialog.FileName;
    TreeButtonedEdit.Text := xFileName;
    TSetting.WriteString('config.sys.tree_list_filename',xFileName);
    xStr := TStringList.Create;
    try
      _SetSave(xStr);
      xStr.SaveToFile(xFileName);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

procedure TMainForm.ListButtonedEditRightButtonClick(Sender: TObject);
begin
  ActionLoadList.Execute;
end;

procedure TMainForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  xS: String;
  xTreeNode: TTreeNode;
begin
  xTreeNode := TreeView.GetNodeAt(X,Y);
  SelectedTreeNode := xTreeNode;
  if Assigned(xTreeNode) then
  begin
    TreeView.BeginDrag(False);
    xS := GetPathTree(TreeView.Selected);
    SetStatusEvent('Дерево: ' + xS);
  end;
end;

procedure TMainForm.ListBoxClick(Sender: TObject);
var
  xS: String;
  xItemIndex: Integer;
begin
  xItemIndex := ListBox.ItemIndex;
  if xItemIndex >= 0 then
  begin
    xS := ListBox.Items[xItemIndex];
    GetInfoFilmParser(xS);
  end;
end;

procedure TMainForm.ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xS: String;
  xItemIndex: Integer;
begin
  xItemIndex := ListBox.ItemIndex;
  if xItemIndex >= 0 then
  begin
    xS := ListBox.Items[xItemIndex];
    SetStatusEvent('Список: ' + xS);
    ListBox.BeginDrag(False);
  end;
end;

procedure TMainForm.ActionAddNodeExecute(Sender: TObject);
var
  xValue: String;
  xTreeNode: TTreeNode;
begin
  xValue := InputBox('Наименование категории','Категория:','');
  if not xValue.IsEmpty then
  begin
    if Assigned(SelectedTreeNode) then
    begin
      xTreeNode := TreeView.Items.AddChild(SelectedTreeNode,xValue);
      xTreeNode.Data := Pointer(TREE_NODE_DIR);
      xTreeNode.ImageIndex := 0;
      TreeView.Selected := xTreeNode;
      xTreeNode.Expand(False);
    end else
    begin
      xTreeNode := TreeView.Items.AddChild(nil,xValue);
      xTreeNode.Data := Pointer(TREE_NODE_DIR);
      xTreeNode.ImageIndex := 0;
      TreeView.Selected := xTreeNode;
      xTreeNode.Expand(False);
    end;
  end;
end;

procedure TMainForm.ActionDeleteNodeExecute(Sender: TObject);
var
  xSelected: TTreeNode;
begin
  xSelected := TreeView.Selected;
  if Assigned(xSelected) then
  begin
    TreeView.Items.Delete(xSelected);
  end;
end;

procedure TMainForm.ActionShLeftExecute(Sender: TObject);

  procedure _PathTreeNode(ATreeNode: TTreeNode);
  var
    xTreeNode: TTreeNode;
  begin
    xTreeNode := ATreeNode.GetFirstChild;
    while Assigned(xTreeNode) do
    begin
      case Integer(xTreeNode.Data) of
        TREE_NODE_DIR: begin
          _PathTreeNode(xTreeNode);
          TreeView.Items.Delete(xTreeNode);
        end;
        TREE_NODE_VALUE: begin
          ListBox.Items.Add(xTreeNode.Text);
          TreeView.Items.Delete(xTreeNode);
        end;
      end;
      xTreeNode := ATreeNode.GetNext;
    end;
  end;

var
  xSelected: TTreeNode;
begin
  xSelected := TreeView.Selected;
  if Assigned(xSelected) then
  begin
    case Integer(xSelected.Data) of
      TREE_NODE_DIR: begin
        _PathTreeNode(xSelected);
        TreeView.Items.Delete(xSelected);
      end;
      TREE_NODE_VALUE: begin
        ListBox.Items.Add(xSelected.Text);
        TreeView.Items.Delete(xSelected);
      end;
    end;
  end;
end;

function TMainForm.SetShRight(const AValue: String): Boolean;

  function _AddTreeNode(const AValue: String): Boolean;
  var
    xTreeNode: TTreeNode;
  begin
    if Assigned(SelectedTreeNode) then
    begin
      xTreeNode := TreeView.Items.AddChild(SelectedTreeNode,AValue);
      xTreeNode.Data := Pointer(TREE_NODE_VALUE);
      xTreeNode.ImageIndex := 5;
      xTreeNode.SelectedIndex := 5;
      TreeView.Selected := xTreeNode;
      xTreeNode.Expand(False);
      Result := True;
    end else
    begin
      xTreeNode := TreeView.Items.AddChild(nil,AValue);
      xTreeNode.Data := Pointer(TREE_NODE_VALUE);
      xTreeNode.ImageIndex := 5;
      xTreeNode.SelectedIndex := 5;
      TreeView.Selected := xTreeNode;
      xTreeNode.Expand(False);
      Result := True;
    end;
  end;

  function _AddTreeNodeInfoFilmParser(const AInfoFilmParser: TInfoFilmParser; const AValue: String): Boolean;
  var
    xS: String;
    xTreeNode, xTreeNodeDir: TTreeNode;
    xPaths: TStrings;
  begin
    xTreeNodeDir := nil;
    if RadioButtonAuthor.Checked then
    begin
      xTreeNode := Self.FindTreeNode(nil,AInfoFilmParser.Author);
      if Assigned(xTreeNode) then
      begin
        xTreeNodeDir := xTreeNode;
      end else
      begin
        xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,AInfoFilmParser.Author);
        xTreeNode.Data := Pointer(TREE_NODE_DIR);
        xTreeNode.ImageIndex := 0;
        TreeView.Selected := xTreeNode;
        xTreeNode.Expand(False);
        xTreeNodeDir := xTreeNode;
      end;
    end else if RadioButtonYear.Checked then
    begin
      xTreeNode := Self.FindTreeNode(nil,AInfoFilmParser.Year);
      if Assigned(xTreeNode) then
      begin
        xTreeNodeDir := xTreeNode;
      end else
      begin
        xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,AInfoFilmParser.Year);
        xTreeNode.Data := Pointer(TREE_NODE_DIR);
        xTreeNode.ImageIndex := 0;
        TreeView.Selected := xTreeNode;
        xTreeNode.Expand(False);
        xTreeNodeDir := xTreeNode;
      end;
    end else if RadioButtonPath.Checked then
    begin
      xPaths := TStringList.Create;
      try
        SetParserPath(xPaths,AInfoFilmParser.Path);
        xTreeNode := nil;
        for xS in xPaths do
        begin
          xTreeNode := Self.FindTreeNode(xTreeNode,xS);
          if Assigned(xTreeNode) then
          begin
            xTreeNodeDir := xTreeNode;
          end else
          begin
            xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,xS);
            xTreeNode.Data := Pointer(TREE_NODE_DIR);
            xTreeNode.ImageIndex := 0;
            TreeView.Selected := xTreeNode;
            xTreeNode.Expand(False);
            xTreeNodeDir := xTreeNode;
          end;
        end;
      finally
        FreeAndNil(xPaths);
      end;
    end;

    xTreeNode := TreeView.Items.AddChild(xTreeNodeDir,AValue);
    xTreeNode.Data := Pointer(TREE_NODE_VALUE);
    xTreeNode.ImageIndex := 5;
    xTreeNode.SelectedIndex := 5;
    TreeView.Selected := xTreeNode;
    xTreeNode.Expand(False);
    Result := True;
  end;

var
  xInfoFilmParser: TInfoFilmParser;
begin
  Result := False;
  if not AValue.IsEmpty then
  begin
    SetInfoFilmParserClear;
    xInfoFilmParser := GetInfoFilmParser(AValue);

    if Assigned(SelectedTreeNode) and (Integer(SelectedTreeNode.Data) = TREE_NODE_VALUE) then
      SelectedTreeNode := SelectedTreeNode.Parent;

    if CheckBoxParser.Checked then
      Result := _AddTreeNodeInfoFilmParser(xInfoFilmParser,AValue)
    else
      Result := _AddTreeNode(AValue);
  end;
end;


procedure TMainForm.ActionShRightExecute(Sender: TObject);
var
  xS: String;
  xIndex: Integer;
begin
  xIndex := ListBox.ItemIndex;
  if xIndex >= 0 then
  begin
    xS := ListBox.Items[xIndex];
    if SetShRight(xS) then
    begin
      ListBox.Items.Delete(xIndex);
      ListBox.SetFocus;
    end;
  end;
end;

procedure TMainForm.CheckBoxParserClick(Sender: TObject);
begin
  TSetting.WriteBool('config.sys.is_parser',CheckBoxParser.Checked);
end;

procedure TMainForm.TreeButtonedEditRightButtonClick(Sender: TObject);
begin
  ActionLoadTree.Execute;
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  xTreeNode: TTreeNode;
begin
  xTreeNode := TreeView.Selected;
  SetInfoFilmParserClear;
  if Integer(xTreeNode.Data) = TREE_NODE_VALUE then
    GetInfoFilmParser(xTreeNode.Text);
end;

procedure TMainForm.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  xS: String;
  xItemIndex: Integer;
  xTargetNode, xSourceNode: TTreeNode;
begin
  if (Source = TreeView) then
    with TreeView do
    begin
      xTargetNode := GetNodeAt(X,Y); // Get target node
      xSourceNode := Selected;
      if (xTargetNode = nil) then
      begin
        EndDrag(False);
        Exit;
      end;

      if Integer(xTargetNode.Data) = TREE_NODE_DIR then
        xSourceNode.MoveTo(xTargetNode,naAddChildFirst)
      else
        EndDrag(False);
    end;

    if (Source = ListBox) then
    begin
      xItemIndex := ListBox.ItemIndex;
      xS := ListBox.Items[xItemIndex];
      if not xS.IsEmpty then
        if SetShRight(xS) then
        begin
          ListBox.Items.Delete(xItemIndex);
          ListBox.EndDrag(False);
        end;
    end;

end;

procedure TMainForm.TreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  xTargetNode, xSourceNode: TTreeNode;
begin

  if (Source = TreeView) then
    with TreeView do
    begin
      xTargetNode := GetNodeAt(X,Y); // Get target node
      xSourceNode := Selected;
      Accept := Assigned(xTargetNode) and (xTargetNode <> xSourceNode);
    end;

  if (Source = ListBox) then
    Accept := True;

end;

end.
