unit BackProp;

// (c) BaseGroup Lab. 2000
//
// Нейронная сеть, обучаемая по принципу обратного
// распространения ошибки. Решение проблемы XOR.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, NeuralBaseComp, NeuralBaseTypes, ComCtrls, Spin;

type
  TForm1 = class(TForm)
    NeuralNetBP: TNeuralNetBP;
    btnTeach: TButton;
    Memo: TMemo;
    prbEpoch: TProgressBar;
    speEpochCount: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    sttError: TStaticText;
    procedure btnTeachClick(Sender: TObject);
    procedure NeuralNetBPEpochPassed(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// Инициализация сети
procedure TForm1.FormActivate(Sender: TObject);
var
  xInputVector: TVectorFloat;   // Входной вектор
  xOutputVector: TVectorFloat;  // Выходой вектор
begin
  // Очистить сведения о примерах
  NeuralNetBP.ResetPatterns;
  SetLength(xInputVector, 2);
  SetLength(xOutputVector, 1);

  // Добавление к сети примеры для обучения
  // Обучаем сеть распознование функции XOR
  //
  //  1 значение | 2 значение | Результат
  //  -----------------------------------
  //      1           1            0
  //      0           1            1
  //      1           0            1
  //      0           0            0

  xInputVector[0] := 1;
  xInputVector[1] := 1;
  xOutputVector[0] := 0;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 1;
  xInputVector[1] := 0;
  xOutputVector[0] := 1;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 0;
  xInputVector[1] := 1;
  xOutputVector[0] := 1;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);

  xInputVector[0] := 0;
  xInputVector[1] := 0;
  xOutputVector[0] := 0;
  NeuralNetBP.AddPattern(xInputVector, xOutputVector);
end;

// Обучение сети
procedure TForm1.btnTeachClick(Sender: TObject);
var
  xInputVector: TVectorFloat;   // Входной вектор
begin
  NeuralNetBP.EpochCount := speEpochCount.Value;
  prbEpoch.Max := NeuralNetBP.EpochCount;
  prbEpoch.Position := 0;

  // Запуск процесса обучения (offline)
  NeuralNetBP.TeachOffLine;

  SetLength(xInputVector, 2);
  Memo.Lines.Clear;

  // Вывод результата
  xInputVector[0] := 1;                 // Даем информацию на вход
  xInputVector[1] := 1;                 // Даем информацию на вход
  NeuralNetBP.Compute(xInputVector);    // Прогон сети вперед
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // Показать результат

  xInputVector[0] := 1;
  xInputVector[1] := 0;
  NeuralNetBP.Compute(xInputVector);    // Прогон сети вперед
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // Показать результат

  xInputVector[0] := 0;
  xInputVector[1] := 1;
  NeuralNetBP.Compute(xInputVector);    // Прогон сети вперед
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // Показать результат

  xInputVector[0] := 0;
  xInputVector[1] := 0;
  NeuralNetBP.Compute(xInputVector);    // Прогон сети вперед
  Memo.Lines.Add(FloatToStr(xInputVector[0]) + ' XOR ' +
                 FloatToStr(xInputVector[1]) + ' = ' +
                 FloatToStr(NeuralNetBP.Output[0]));     // Показать результат
end;

// Отображение процесса обучения
procedure TForm1.NeuralNetBPEpochPassed(Sender: TObject);
begin
  prbEpoch.Position := prbEpoch.Position + 1;
  sttError.Caption := FloatToStr(NeuralNetBP.TeachError);
  Application.ProcessMessages;
end;


end.
