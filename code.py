import torch
import torchvision
import torchvision.transforms as transforms
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import math
import tqdm

class Net(nn.Module):
    def __init__(self):
        super().__init__()
        self.features = nn.Sequential(
            nn.Conv2d(3, 64, kernel_size=3, padding=1),
            nn.BatchNorm2d(64),
            nn.ReLU(True),
            nn.MaxPool2d(2,2),
            nn.Dropout(0.2),

            nn.Conv2d(64, 128, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(128),
            nn.ReLU(True),
            nn.MaxPool2d(2,2),
            nn.Dropout(0.2),

            nn.Conv2d(128, 256, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(256),
            nn.ReLU(True),
            nn.Conv2d(256, 256, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(256),
            nn.ReLU(True),
            nn.MaxPool2d(2,2),
            nn.Dropout(0.2),

            nn.Conv2d(256, 512, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(512),
            nn.ReLU(True),
            nn.Conv2d(512, 512, kernel_size=3, stride=1, padding=1),
            nn.BatchNorm2d(512),
            nn.ReLU(True),
            nn.MaxPool2d(2,2),
            nn.Dropout(0.2),
        )

        self.classifier = nn.Sequential(
            nn.Flatten(),
            nn.Linear(512 * 2 * 2, 4096),
            nn.ReLU(True),
            nn.Dropout(0.2),
            nn.Linear(4096, 4096),
            nn.ReLU(True),
            nn.Dropout(0.2),
            nn.Linear(4096, 1000),
            nn.ReLU(True),
            nn.Dropout(0.5),
            nn.Linear(1000, 10)
        )

    def forward(self, x):
        x = self.features(x)
        x = self.classifier(x)
        return x

def get_cifar10():
    train_transform = transforms.Compose(
        [
            transforms.RandomCrop(32, padding=4),
            transforms.RandomHorizontalFlip(),
            transforms.ToTensor(),
            transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
        ]
    )
    train_dataset = torchvision.datasets.CIFAR10(
        "./data/CIFAR10", train=True, transform=train_transform, download=True
    )

    test_transform = transforms.Compose(
        [
            transforms.ToTensor(),
            transforms.Normalize((0.5, 0.5, 0.5), (0.5, 0.5, 0.5))
        ]
    )
    test_dataset = torchvision.datasets.CIFAR10(
        "./data/CIFAR10", train=False, transform=test_transform, download=True
    )

    return train_dataset, test_dataset

def train_and_test(net, train_loader, test_loader, optimiser):
    net.train()

    running_loss = []

    for inputs, labels in tqdm.tqdm(train_loader):
        inputs = inputs.cuda()
        labels = labels.cuda()

        optimiser.zero_grad()

        outputs = net(inputs)
        loss = F.cross_entropy(outputs, labels)
        loss.backward()
        optimiser.step()

        running_loss.append(loss.item())

    avg_loss = sum(running_loss) / len(running_loss)
    print("Training average loss: {}".format(avg_loss))

    net.eval()

    running_loss = 0.0
    running_correct = 0

    for inputs, labels in test_loader:
        with torch.no_grad():
            inputs = inputs.cuda()
            labels = labels.cuda()

            outputs = net(inputs)
            running_loss += F.cross_entropy(outputs, labels, reduction = "sum")

            outputs = outputs.max(1)[1]
            running_correct += outputs.eq(labels.view_as(outputs)).sum().item()

    avg_loss = running_loss / len(test_loader.dataset)
    percent_correct = running_correct * 100.0 / len(test_loader.dataset)
    print("Testing loss: {}, Accuracy: {}/10000 ({}%)".format(avg_loss, running_correct, percent_correct))

    return percent_correct

train_dataset, test_dataset = get_cifar10()

train_loader = torch.utils.data.DataLoader(
    train_dataset, batch_size=128, shuffle=True
)
test_loader = torch.utils.data.DataLoader(
    test_dataset, batch_size=128, shuffle=False
)

net = Net()
net = net.cuda()
optimiser = optim.SGD(net.parameters(), lr = 0.05, momentum = 0.9, weight_decay=5e-4)
scheduler = optim.lr_scheduler.StepLR(optimiser, step_size = 30, gamma = 0.1)

best = 0.0
for epoch in range(200):
    print("Epoch: {}, LR: {}".format(epoch, optimiser.param_groups[0]['lr']))
    p = train_and_test(net, train_loader, test_loader, optimiser)
    if p > best:
        torch.save(net.state_dict(), "cifar_net.pth")
        best = p
    print("(Best: {}%)".format(best))
    scheduler.step()

torch.save(net.state_dict(), "cifar_net.pth")
