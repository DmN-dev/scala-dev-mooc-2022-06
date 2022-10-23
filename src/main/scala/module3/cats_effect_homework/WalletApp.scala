package module3.cats_effect_homework

import cats.effect.{FiberIO, IO, IOApp}
import cats.implicits._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletApp extends IOApp.Simple {
  def topUpWallet(wallet: Wallet[IO], amount: BigDecimal, interval: FiniteDuration): IO[Unit] = {
    for {
      _ <- IO.sleep(interval)
      _ <- wallet.topup(amount)
    } yield ()
  }

  def printBalanceInfinite(wallet1: Wallet[IO], wallet2: Wallet[IO], wallet3: Wallet[IO]): IO[Unit] = {
    def printBalance(wallet1: Wallet[IO], wallet2: Wallet[IO], wallet3: Wallet[IO]): IO[Unit] = {
      for {
        balance1 <- wallet1.balance
        balance2 <- wallet2.balance
        balance3 <- wallet3.balance
        _ <- IO.println(s"Wallet1 balance=$balance1, Wallet2 balance=$balance2, Wallet3 balance=$balance3")
      } yield ()
    }
    for {
      _ <- IO.sleep(1.second)
      _ <- printBalance(wallet1, wallet2, wallet3)
    } yield ()
  }

  def cancelFiber(fiber: FiberIO[_], msg: String): IO[Unit] = IO.println(msg) *> fiber.cancel

  def run: IO[Unit] = {
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      fiber1 <- topUpWallet(wallet1, 100, 100.millis).foreverM.start
      fiber2 <- topUpWallet(wallet2, 100, 500.millis).foreverM.start
      fiber3 <- topUpWallet(wallet3, 100, 2000.millis).foreverM.start
      fiber4 <- printBalanceInfinite(wallet1, wallet2, wallet3).foreverM.start
      _ <- (IO.sleep(1.second) *> IO.readLine).iterateUntil(s => s.isBlank || s.nonEmpty)
      _ <-
        cancelFiber(fiber1, "Fiber1 is cancelling") *>
          cancelFiber(fiber2, "Fiber2 is cancelling") *>
            cancelFiber(fiber3, "Fiber3 is cancelling") *>
              cancelFiber(fiber4, "Fiber4 is cancelling")
      _ <- IO.println("The End!")
    } yield ()
  }
}
