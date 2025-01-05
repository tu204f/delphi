unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMX.TabControl, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Lb.ParamPosition,
  UnitParamPositionFrame;

type
  TMainForm = class(TForm)
    ListBoxPosition: TListBox;
    TextTitlePosition: TText;
    LayoutClient: TLayout;
    LayoutPosition: TLayout;
    TabControl: TTabControl;
    TabItemSource: TTabItem;
    MemoSource: TMemo;
    TabItemParamPosition: TTabItem;
    LayoutParamPosition: TLayout;
    procedure ListBoxPositionChange(Sender: TObject);
  private
    ParamPositionFrame: TParamPositionFrame;
    procedure SetNameFiles;
  public
    ParamPosition: TParamPosition;
    NameFiles: TStrings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils;

//procedure SetPathPositions(const APathDir: String; ANameFiles: TStrings);
//var
//  xFileList: TStringDynArray;
//  S: string;
//begin
//  ANameFiles.Clear;
//  xFileList := TDirectory.GetFiles(APathDir);
//  for S in xFileList do
//    ANameFiles.Add(S);
//end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  NameFiles := TStringList.Create;

//  SetPathPositions(
//    'd:\work\git\delphi\sample\bot_rsi\bq_rsi_2\app\bin\data\',
//    NameFiles
//  );
  SetNameFiles;

  ParamPosition := TParamPosition.Create;

  ParamPositionFrame := TParamPositionFrame.Create(nil);
  ParamPositionFrame.Parent := LayoutParamPosition;
  ParamPositionFrame.Align := TAlignLayout.Client;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ParamPositionFrame);
  FreeAndNil(ParamPosition);
  FreeAndNil(NameFiles);
  inherited;
end;

procedure TMainForm.SetNameFiles;
begin
//  for var S in NameFiles do
//    ListBoxPosition.Items.Add(ExtractFileName(S));

  for var i := 0 to 66 do
    ListBoxPosition.Items.Add('posiotn_' + i.ToString + '.txt');
end;

procedure TMainForm.ListBoxPositionChange(Sender: TObject);
var
  xStr: TStrings;
  xFileName: String;
  xIndex: Integer;
begin
  xIndex := ListBoxPosition.ItemIndex;
  xFileName :=
    'd:\work\git\delphi\sample\bot_rsi\bq_rsi_2\app\bin\data\' +
    ListBoxPosition.Items[xIndex];
  xStr :=  TStringList.Create;
  try
    xStr.LoadFromFile(xFileName);
    SetLoadParamPosition(xStr, ParamPosition);
    ParamPositionFrame.ParamPosition := ParamPosition;
    MemoSource.Lines.Assign(xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

end.
