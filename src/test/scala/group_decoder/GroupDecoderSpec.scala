package group_decoder

class GroupDecoderSpec
    extends chisel3.iotesters.PeekPokeTester(
      new GroupDecoder(GroupDecoderParams(512, 32))
    ) {
    // 测试用例可以在这里编写
}
