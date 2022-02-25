package vm.logic;

public interface Interactor {
  static final long serialVersionUID=222L;
  
  // Initiative: client
  // should also contain a static method new_intercator
  // returning an Interactor or null if its private contructor fails
  
  /**
   * Initiative: client
   * data is obtained from this Interactor
   * this usually triggers some computation in this interactor
   * and might involve the interactor calling
   * methods within its initiative
   */
  public Object ask_interactor();
  
  /**
   * Initiative: client
   *   send a message/command/configuration request to this Interactor
   * Result: 
   *   receive back some form of acknowledgement or status indication
   */
  public Object tell_interactor(Object message);
  
  /**
   * Initiative: client or the interactor itself
   * Result: 
   *   stops this Interactor
   *   this can discard it or make it available for reuse
   */
  public void stop_interactor();
  
  /**
   * Initiative: this Interactor
   * 
   * Result: 
   *   a(possibly) performs some action on it and
   *   returns some form of acknowledgement
   * 
   *   this method is meant to be private - i.e.
   *   only the interactor should call it
   */
  public Object interactor_handle(Object message);
  
}
